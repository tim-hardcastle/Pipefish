package text

import (
	"fmt"
	"strings"

	"github.com/mattn/go-runewidth"
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

func (md *Markdown) Render(r []rune) string {
	return md.RenderLeftPad("", r)
}

func (md *Markdown) RenderLeftPad(pad string, r []rune) string {
	leftMarginWidth := runewidth.StringWidth(md.leftMargin)
	var StringBuilder strings.Builder
	sb := &StringBuilder
	ox := 0
	if len(pad) > 0 {
		fmt.Fprint(sb, pad)
		ox = runewidth.StringWidth(pad)
	}  else  {
		fmt.Fprint(sb, md.leftMargin)
		ox = leftMarginWidth
	}
	ix := 0
	r = append(r, 0)
	font := ""
	for ix < len(r) - 1 {
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
		slurp :
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
				ix++
				break slurp
			case '>':
				word = word + ">"
				ix++
				break slurp
			case '-', '/', ' ':
				word = word + string(r[ix])
				ix++
				break slurp
			case '<' :
				if word == "" {
					word = "<"
					ix++
				} else {
					break slurp
				}
			default:
				word = word + string(r[ix])
				ix++
			}
		}
		// We replace things like `**` and `<red>` with suitable control codes.
		replacement := ""
		if replacement, controlCode = replacements[word]; controlCode {
			word = replacement
			if word == RESET {
				font = ""
			} else {
				font = font + word
			}
			if r[ix] == ' ' {
				word = word + " "
				ix++
			}
		}
		// We check if the length of the word puts it over the right margin.
		wordWidth := runewidth.StringWidth(word)
		newOx := ox + wordWidth
		if newOx > md.rightMargin {
			if controlCode {
				fmt.Fprint(sb, word, "\n", RESET, md.leftMargin, font)
				ox = leftMarginWidth
			} else {
				fmt.Fprint(sb, "\n", RESET, md.leftMargin, font, word)
				ox = leftMarginWidth + wordWidth
			}
			
		} else {
			fmt.Fprint(sb, word)
			ox = newOx
		}
	}
	return sb.String()
}

var (
	replacements = map[string]string{
		"<red>":RED,
		"<yellow>":YELLOW,
		"<green>":GREEN,
		"<blue>":BLUE,
		"<purple>":PURPLE,
		"<plain>":RESET,
	}
)