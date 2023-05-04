package token

type TokenType string

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"
	BUILTIN = "BUILTIN"

	// Identifiers + literals
	IDENT   = "IDENT"   // add, foobar, x, y, ...
	INT     = "int"     // 1343456
	FLOAT   = "float64" // 1.23
	STRING  = "string"  // "foo", `bar`
	TRUE    = "true"
	FALSE   = "false"
	COMMENT = "COMMENT" // // foo bar zort troz

	BEGIN = "BEGIN"
	END   = "END"

	// Operators
	ASSIGN     = "="
	CMD_ASSIGN = "=cmd="
	VAR_ASSIGN = "=var="
	DEF_ASSIGN = "=def="
	GVN_ASSIGN = "=gvn="
	LZY_ASSIGN = "=lzy="
	PVR_ASSIGN = "=pvr="
	TYP_ASSIGN = "=typ="

	COLON     = ":"
	NEWLINE   = "\n"
	SEMICOLON = ";"

	AND = "and"
	OR  = "or"
	NOT = "not"

	EQ     = "=="
	NOT_EQ = "!="

	DOUBLESLASH = "//"

	LOG = "LOG"
	IFLOG = "IFLOG"
	MAGIC_IFLOG = "MAGIC_IFLOG"
	PRELOG = "PRELOG"
	AUTOLOG = "AUTOLOG"

	DOTDOT = ".."
	NO_INDENT = "|||"

	COMMA      = ","
	WEAK_COMMA = ",,"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"
	LBRACK = "["
	RBRACK = "]"

	// Headwords

	IMPORT  = "import"
	VAR     = "var"
	CMD     = "cmd"
	DEF     = "def"
	PRIVATE = "private"

	// Keywords
	ELSE    = "else"
	GIVEN   = "given"
	EVAL    = "eval"
	EXEC    = "exec"

	// Imperative shell
	
	LOOP = "loop"
	REQUEST = "request"
	RESPOND = "respond"

	MAGIC_COLON = "MAGIC_COLON"
	WEAK_COLON = "WEAK_COLON"

	GOLANG = "gocode"

	// Streaming operators
	PIPE = "->"
	MAP = ">>"
	FILTER = "?>"

)

type Token struct {
	Type    TokenType
	Literal string
	Line    int
	ChStart int
	ChEnd   int
	Source  string
}

var keywords = map[string]TokenType{
	"true":   TRUE,
	"false":  FALSE,
	"else":   ELSE,

	"loop": LOOP,
	"respond": RESPOND,
	"request": REQUEST,

	"eval":   EVAL,
	"given":  GIVEN,
	"exec":   EXEC,
	"gocode": GOLANG,

	"import":  IMPORT,
	"var":     VAR,
	"cmd":     CMD,
	"def":     DEF,
	"private": PRIVATE,

	"and": AND,
	"or":  OR,
	"not": NOT,

}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

func TokenTypeIsHeadword(t TokenType) bool {
	return t == IMPORT || t == VAR || t == CMD || t == DEF
}
