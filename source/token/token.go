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

	LOG    = "LOG"
	IFLOG  = "IFLOG"
	PRELOG = "PRELOG"

	TRY = "try"

	DOTDOT    = ".."
	NO_INDENT = "|||"

	COMMA      = ","
	WEAK_COMMA = ",,"

	LPAREN = "("
	RPAREN = ")"
	LBRACE = "{"
	RBRACE = "}"
	LBRACK = "["
	RBRACK = "]"

	NAMESPACE_SEPARATOR = "."

	// Headwords

	IMPORT    = "import"
	VAR       = "var"
	CMD       = "cmd"
	DEF       = "def"
	PRIVATE   = "private"
	LANGUAGES = "languages"
	CONTACTS  = "contacts"

	// Keywords
	ELSE   = "else"
	GIVEN  = "given"
	EVAL   = "eval"
	LOOP   = "loop"
	GOLANG = "gocode"
	GLOBAL = "global"

	// For internal use
	MAGIC_COLON = "MAGIC_COLON"
	WEAK_COLON  = "WEAK_COLON"

	// Streaming operators
	PIPE   = "->"
	MAP    = ">>"
	FILTER = "?>"

	EMDASH = "EMDASH"
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
	"import":    IMPORT,
	"var":       VAR,
	"cmd":       CMD,
	"def":       DEF,
	"private":   PRIVATE,
	"languages": LANGUAGES,
	"contacts":  CONTACTS,

	"true":  TRUE,
	"false": FALSE,
	"else":  ELSE,

	"and": AND,
	"or":  OR,
	"not": NOT,

	"loop": LOOP,

	"eval":   EVAL,
	"given":  GIVEN,
	"gocode": GOLANG,
	"global": GLOBAL,

	"try": TRY,

	"->":  PIPE,
	"---": EMDASH,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

func TokenTypeIsHeadword(t TokenType) bool {
	return t == IMPORT || t == VAR || t == CMD || t == DEF || t == LANGUAGES || t == CONTACTS
}
