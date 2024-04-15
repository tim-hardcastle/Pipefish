package token

type TokenType string

const (
	// Keywords
	ELSE   = "else"
	EVAL   = "eval"
	GIVEN  = "given"
	GLOBAL = "global"
	GOLANG = "gocode"
	LOOP   = "loop"
	TRY    = "try"

	// Headwords
	IMPORT    = "import"
	VAR       = "var"
	CMD       = "cmd"
	DEF       = "def"
	PRIVATE   = "private"
	LANGUAGES = "languages"
	CONTACTS  = "contacts"
	CONST     = "const"
	TYPES     = "types"

	// Special operations
	AND = "and"
	OR  = "or"
	NOT = "not"

	EMDASH  = "---"
	EQ      = "=="
	FILTER  = "?>"
	MAPPING = ">>"
	NOT_EQ  = "!="
	PIPE    = "->"

	// Assignment operators
	ASSIGN     = "="
	CMD_ASSIGN = "=CMD="
	DEF_ASSIGN = "=DEF="
	GVN_ASSIGN = "=GVN="
	LZY_ASSIGN = "=LZY="
	PVR_ASSIGN = "=PVR="
	TYP_ASSIGN = "=TYP="
	VAR_ASSIGN = "=VAR="

	// The protected punctuation.
	COLON               = ":"
	DOTDOT              = ".."
	COMMA               = ","
	LPAREN              = "("
	LBRACE              = "{"
	LBRACK              = "["
	NAMESPACE_SEPARATOR = "."
	NEWLINE             = "\n"
	RBRACE              = "}"
	RBRACK              = "]"
	RPAREN              = ")"
	SEMICOLON           = ";"

	// Literals, comments, and identifiers.
	COMMENT = "COMMENT"
	FLOAT   = "FLOAT LITERAL"
	IDENT   = "IDENT"
	INT     = "INTEGER LITERAL"
	STRING  = "STRING LITERAL"

	// False and true.
	FALSE = "false"
	TRUE  = "true"

	// For internal use.
	BEGIN       = "BEGIN"
	BUILTIN     = "BUILTIN"
	END         = "END"
	EOF         = "EOF"
	IFLOG       = "IFLOG"
	ILLEGAL     = "ILLEGAL"
	LOG         = "LOG"
	MAGIC_COLON = "MAGIC COLON"
	NO_INDENT   = "|||"
	PRELOG      = "PRELOG"
	WEAK_COLON  = "WEAK COLON"
	WEAK_COMMA  = ",,"
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
	// Keywords.
	"else":   ELSE,
	"eval":   EVAL,
	"given":  GIVEN,
	"gocode": GOLANG,
	"global": GLOBAL,
	"loop":   LOOP,
	"try":    TRY,

	// Headwords.
	"const":     CONST,
	"contacts":  CONTACTS,
	"cmd":       CMD,
	"def":       DEF,
	"import":    IMPORT,
	"languages": LANGUAGES,
	"private":   PRIVATE,
	"types":     TYPES,
	"var":       VAR,

	//Special operators.
	"and": AND,
	"or":  OR,
	"not": NOT,
	"->":  PIPE,
	">>":  MAPPING,
	"?>":  FILTER,
	"==":  EQ,
	"!=":  NOT_EQ,
	"---": EMDASH,

	// False and true.
	"true":  TRUE,
	"false": FALSE,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

func TokenTypeIsHeadword(t TokenType) bool {
	return t == IMPORT || t == VAR || t == CMD || t == DEF || t == LANGUAGES || t == CONTACTS || t == TYPES || t == CONST
}
