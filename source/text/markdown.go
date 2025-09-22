package text

import (
	"fmt"
	"regexp"
	"strings"
)

type Markdown struct {
	leftMargin  string              // Left margin as a string.
	rightMargin int                 // Right margin as a width to render to.
	highlight   func([]rune) string // Syntax highlighter for code in fenced blocks.
}

func NewMarkdown(leftMargin string, rightMargin int, highlight func([]rune) string) *Markdown {
	md := Markdown{leftMargin: leftMargin, rightMargin: rightMargin, highlight: highlight}
	return &md
}

func (md *Markdown) Render(text []string) string {
	return md.RenderLeftPad("", text)
}

func (md *Markdown) RenderLeftPad(pad string, text []string) string {
	leftMarginWidth := colorlessLength(md.leftMargin)
	var StringBuilder strings.Builder
	sb := &StringBuilder
	ox := 0
	if len(pad) > 0 {
		fmt.Fprint(sb, pad)
		ox = colorlessLength(pad)
	} else {
		fmt.Fprint(sb, md.leftMargin)
		ox = leftMarginWidth
	}
	font := ""
	blockQuote := false
line:
	for i, s := range text {
		// A number of empty lines adds up to one empty line.
		if s == "" && (i > 0 || text[i-1] != "") {
			fmt.Fprint(sb, "\n", md.leftMargin)
			ox = leftMarginWidth
		}
		// We do the headings.
		heading := captureHeading.FindString(s)
		if heading != "" {
			if i > 0 {
				fmt.Fprint(sb, "\n", RESET, md.leftMargin, font)
			}
			headingIndex := len(heading) - 2
			decoration := deco[headingIndex]
			font := style[headingIndex]
			temp := md.leftMargin // Ouch.
			md.leftMargin = ""
			textIs := md.Render([]string{s[len(heading):]})
			md.leftMargin = temp
			lengthIs := colorlessLength(textIs)
			paddingLength := md.rightMargin - (leftMarginWidth + 6 + lengthIs)
			if paddingLength < 0 {
				return (ErrorFont("Can't render markdown."))
			}
			padding := strings.Repeat(decoration, paddingLength)
			leftPaddingIs := strings.Repeat(decoration, 4)
			headingIs := leftPaddingIs + " " + textIs + padding + "\n" + md.leftMargin
			fmt.Fprint(sb, applyFont(headingIs, font))
			ox = leftMarginWidth
			continue line

		}
		ix := 0
		sidebar := "" // Contains the block quote sidebar if it exists.
		// We may need to turn block quotes on.
		if Head(s, "> ") {
			ix = ix + 2
			sidebar = "  ‖ "
			if !blockQuote {
				blockQuote = true
				fmt.Fprint(sb, "\n", RESET, md.leftMargin)
				ox = leftMarginWidth
			}
			if ox == leftMarginWidth {
				fmt.Fprint(sb, sidebar, font)
				ox = ox + len(sidebar)
			}
		} else {
			if blockQuote {
				fmt.Fprint(sb, "\n", RESET, md.leftMargin, font)
				ox = leftMarginWidth
				blockQuote = false
			}
		}
		r := []rune(s)
		r = append(r, 0)
		currentStyle := ""
		for ix < len(r)-1 {
			// We remove leading whitespace.
			for r[ix] == ' ' {
				ix++
			}
			// We slurp one word
			word := ""
			// A kludge on a kludge. If we find a control code with a space after it
			// we want to put the space into the text, except that might push a <plain>
			// control code onto the next line together with the space, which screws things
			// up. Therefore, when all we have is a control code which may have a space
			// after it, we don't try to justify it because no-one can tell anyway.
			controlCode := false
		slurp:
			for {
				switch r[ix] {
				case '\n':
					ix++
					if r[ix] == '\n' {
						word = word + "\n\n"
						for r[ix] == '\n' {
							ix++
						}
					} else {
						word = word + " "
					}
				case 0:
					word = word + " "
					ix++
					break slurp
				case '>':
					word = word + ">"
					ix++
					break slurp
				case '/':
					word = word + string(r[ix])
					ix++
					if word != "</" {
						break slurp
					}
				case '-', ' ':
					word = word + string(r[ix])
					ix++
					break slurp
				case '<':
					if word == "" {
						word = "<"
						ix++
					} else {
						break slurp
					}
				case '`':
					if word == "" {
						word = "`"
						ix++
						break slurp
					} else {
						break slurp
					}
				case '*':
					if word == "" {
						for r[ix] == '*' {
							word = word + "*"
							ix++
						}
						if currentStyle == "`" && r[ix] == ' ' {
							word = word + " "
						}
						break slurp
					} else {
						break slurp
					}
				default:
					word = word + string(r[ix])
					ix++
				}
			}
			inlineCode := currentStyle == "`"
			if word != "" && currentStyle == word {
				if word == "`" {
					controlCode = true
					word = RESET
				} else {
					controlCode = true
					word = RESET_BOLD + RESET_ITALIC
				}
				currentStyle = ""
			} else {
				if word == "`" || word == "*" || word == "**" || word == "***" {
					currentStyle = word
					controlCode = true
				}
			}
			// We replace things like `<R>` with suitable control codes.
			if replacement, ok := replacements[word]; ok && !inlineCode {
				word = replacement
				controlCode = true
			}
			wordWidth := len(word)
			if controlCode {
				if word == RESET {
					font = ""
				} else {
					font = font + word
				}
				if r[ix] == ' ' {
					wordWidth = 1
					word = word + " "
					ix++
				} else {
					wordWidth = 0
				}
			}
			// We check if the length of the word puts it over the right margin.
			newOx := ox + wordWidth
			if newOx >= md.rightMargin {
				if controlCode {
					fmt.Fprint(sb, word, RESET, "\n", md.leftMargin, sidebar, font)
					ox = leftMarginWidth + len(sidebar)
				} else {
					fmt.Fprint(sb, RESET, "\n", md.leftMargin, sidebar, font, word)
					ox = leftMarginWidth + len(sidebar) + len(word)
				}
			} else {
				fmt.Fprint(sb, word)
				ox = newOx
			}
		}
	}
	return sb.String()
}

func stripColors(s string) string {
	return string(stripColorCodes.ReplaceAllString(s, ""))
}

func colorlessLength(s string) int {
	return len(stripColors(s))
}

func applyFont(s, font string) string {
	return font + findResets.ReplaceAllString(s, RESET+font) + RESET
}

var (
	stripColorCodes, _ = regexp.Compile("\033\\[[0-9;]*m")
	findResets, _      = regexp.Compile("\033\\[[0|39|49|22|23|24]m")
	captureHeading, _  = regexp.Compile("^#{1,4} ")
	deco               = []string{"≡", "═", "―", "┈"}
	style              = []string{BOLD + ITALIC, BOLD, ITALIC, ""}
	replacements       = map[string]string{
		"<R>": RED,
		"<Y>": YELLOW,
		"<G>": GREEN,
		"<C>": CYAN,
		"<B>": BLUE,
		"<P>": PURPLE,
		"</>": RESET_FOREGROUND,
		"*":   ITALIC,
		"**":  BOLD,
		"***": BOLD + ITALIC,
		"`":   RESET + GRAY_BACKGROUND + CYAN,
	}
)
