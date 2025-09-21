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
	for ix < len(r) - 1 {
		// We remove leading whitespace.
		for r[ix] == ' ' {
			ix++
		}
		// We slurp one word
		word := ""
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
			case '-', '/', ' ':
				word = word + string(r[ix])
				ix++
				break slurp
			default:
				word = word + string(r[ix])
				ix++
			}
		}
		if replacement, ok := replacements[word]; ok {
			word = replacement
		}
		// We check if the length of the word puts it over the right margin.
		wordWidth := runewidth.StringWidth(word)
		newOx := ox + wordWidth
		if newOx > md.rightMargin {
			fmt.Fprint(sb, "\n", md.leftMargin, word)
			ox = leftMarginWidth + wordWidth
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