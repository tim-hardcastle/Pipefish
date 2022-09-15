package token

type TokenType string

const (
	ILLEGAL = "ILLEGAL"
	EOF     = "EOF"
	BUILTIN = "BUILTIN"

	// Identifiers + literals
	IDENT = "IDENT" // add, foobar, x, y, ...
	INT   = "int"   // 1343456
	FLOAT = "float" // 1.23
	STRING = "string" // "foo", `bar`
	TRUE     = "true"
	FALSE    = "false"
	COMMENT = "COMMENT" // // foo bar zort troz

	BEGIN = "BEGIN"
	END = "END"

	// Operators
	ASSIGN      = "="
	CMD_ASSIGN	= "=cmd="
	VAR_ASSIGN	= "=var="
	DEF_ASSIGN	= "=def="
	GVN_ASSIGN  = "=gvn="
	PVR_ASSIGN  = "=pvr="
	TYP_ASSIGN  = "=typ="

	COLON     = ":"
	DOT       = "."
	NEWLINE   = "\n"
	SEMICOLON = ";"

	AND      = "and"
	OR       = "or"
	NOT		= "not"

	EQ     = "=="
	NOT_EQ = "!="

	DOUBLESLASH = "//"

	DOTDOT = ".."

	RIGHTARROW = "->"

	NO_INDENT = "|||"

	COMMA     = ","
	WEAK_COMMA = ",,"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"
	LBRACK = "["
	RBRACK = "]"

	// Headwords

	IMPORT 	= "import"
	VAR     = "var"
	CMD     = "cmd"
	DEF     = "def"
	PRIVATE = "private"

	// Keywords
	RETURN	 = "return"
	ELSE     = "else"
	GIVEN	 = "given"
	EVAL     = "eval"
	EXEC	 = "exec"

	MAGIC_COLON = "MAGIC_COLON"
)

type Token struct {
	Type    TokenType
	Literal string
	Line    int
	ChStart int
	ChEnd	int
	Source	string
} 


var keywords = map[string] TokenType{
	"true":   TRUE,
	"false":  FALSE,
	"else":   ELSE,
	"return": RETURN,

	"eval":		EVAL,
	"given":	GIVEN,
	"exec":		EXEC,

	"import":	IMPORT,
	"var":		VAR,
	"cmd":		CMD,
	"def":		DEF,
	"private":	PRIVATE,

	"and":		AND,
	"or":		OR,
	"not":		NOT,

	"=cmd=":	CMD_ASSIGN,
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
