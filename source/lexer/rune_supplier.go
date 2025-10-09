package lexer

// We want to be able to use the same functions for slurping up e.g. string literals
// here and in the REPL highlighter, for sanity. The RuneSupplier gives us something
// simpler than a lexer that we can use in the REPL highlighter and inside of the
// lexer.
type RuneSupplier struct {
	code        []rune
	pos         int 
	lineNo      int
	lineStart   int
}

func NewRuneSupplier(code []rune) *RuneSupplier {
	return &RuneSupplier{code: code, lineNo: 1}
}

func (rs *RuneSupplier) CurrentRune() rune {
	if rs.pos < len(rs.code) {
		return rs.code[rs.pos]
	}
	return 0
}

func (rs *RuneSupplier) PeekRune() rune {
	if rs.pos + 1 < len(rs.code) {
		return rs.code[rs.pos+1]
	}
	return 0
}

func (rs *RuneSupplier) LastRune() rune {
	if rs.pos > 0 {
		return rs.code[rs.pos-1]
	}
	return 0
}

func (rs *RuneSupplier) Next() {
	if rs.pos >= len(rs.code) {
		return
	}
	if rs.pos >= 0 && rs.code[rs.pos] == '\n' {
		rs.lineNo++
		rs.lineStart = rs.pos + 1
	}
	rs.pos++
}

func (rs *RuneSupplier) Position() (int, int) {
	return rs.lineNo, rs.pos - rs.lineStart
}
