## token.nim - C's tokens
import std/[tables]


type Token* = enum
  TNul=0, TNewLine=int('\n'),
#    ' '             !      "
  TSpace=int(' '), TNot, TDoubleQ,
#    #               $        %        &         '           (          )        *    +      ,       -      .     /
  TBash=int('#'), TDolor, TPercent, TBitAnd, TSignleQ,   TLbracket, TRbracket, TMul, TAdd, TComma, TDash, TDot, TSlash,
#    :                 ;        <      =      >         ?          @ 
  TColon=int(':'), TSemicolon, TLt, TAssign, TGt, TQuestionMark, TMouse, 
#       [                       \               ]            ^        _           `
  TLSquareBrackets=int('['), TBackslash, TRSquareBrackets, TXor, TUnderscore, TGraveAccent,
#         {                  |           }            ~
  TLcurlyBracket=int('{'), TBitOr, TRcurlyBracket, TBitNot, 

  T255=255,

  Kauto="auto", Kfor="for",
  Kgoto="goto", Kif="if", Kinline="inline", 
  Kint="int", Kchar="char", Klong="long", 
  Kregister="register", Krestrict="restrict", 
  Kreturn="return", Kshort="short", Kelse="else", 
  Kenum="enum", Kextern="extern", Kdouble="double", 
  Kdo="do", Kdefault="default", Kfloat="float",
  Ksigned="signed", Ksizeof="sizeof", Kstatic="static", 
  Kstruct="struct", Kswitch="switch", Kcase="case",
  Ktypedef="typedef", Kunion="union", Kcontinue="continue",
  Kunsigned="unsigned", Kvoid="void", Kvolatile="volatile", 
  Kwhile="while", Kconst="const", Kbreak="break",
  K_Alignas="_Alignas", K_Alignof="_Alignof", K_Atomic="_Atomic", 
  K_Bool="_Bool", K_Complex="Complex", K_asm="__asm__"
  K_Decimal128="_Decimal128", K_Decimal32="_Decimal32", 
  K_Decimal64="_Decimal64", K_Generic="_Generic", 
  K_Imaginary="_Imaginary", K_Noreturn="_Noreturn", K_Static_assert="_Static_assert",
  K_Thread_local="_Thread_local"

  TAddAdd="++", TSubSub="--", TArrow="->",
  Tshl="<<", Tshr=">>",
  TGe=">=", TLe="<=",
  TEq="==", TNe="!=",
  TLogicalOr="||",
  TLogicalAnd="&&",
  TAsignAdd="+=",
  TAsignSub="-=",
  TAsignMul="*=",
  TAsignDiv="/=",
  TAsignRem="%=",
  TAsignShl="<<=",
  TAsignShr=">>=",
  TAsignBitAnd="&=",
  TAsignBitOr="|=",
  TAsignBitXor="^=",
  
  TEllipsis2="..",
  TEllipsis="...", # varargs
  TNumberLit="<number>",
  TFloatLit="<float>",
  TCharLit="<char>",
  TIdentifier="<identifier>",
  CPPIdent="<CPPident>",
  TStringLit="<string>",
  TPPNumber="<pp-number>", # pp-token => pp-number, used by preprocessor
  PPPlaceholder="<placeholder>",
  PPSharp="#",
  PPSharpSharp="##",
  PPMacroPop="<pop>",
  TEOF="<EOF>"

type
    PPMacroFlags* = enum
      MOBJ, MFUNC, MBuiltin
    PPMacro* = ref object
      tokens*: seq[TokenV]
      case flags*: PPMacroFlags # if a object like macro?
      of MFUNC:
        params*: seq[string]
        ivarargs*: bool
      of MBuiltin:
        fn*: proc ()
      of MOBJ:
        discard
    ParseFlags* = enum
      PFNormal=1, PFPP=2
    TokenVTags* = enum
      TVNormal, TVSVal, TVIVal, TVFval, TVStr
    # integer tags
    ITag* = enum
      Iint,   Ilong,   Iulong,  Ilonglong, Iulonglong, Iuint
    # also for chars
    # char,   char16,  char32,  wchar
    #  u8       u        U       L
    # float tags
    FTag* = enum
      Fdobule, Ffloat
    TokenV* = ref object
      tok*: Token
      case tags*: TokenVTags
      of TVNormal:
        discard
      of TVSVal:
        s*: string
      of TVFval:
        f*: float
        ftag*: FTag
      of TVIVal:
        i*: int
        itag*: ITag
      of TVStr:
        str*: string
        enc*: uint8

const
  CSkip* = {' ', '\t', '\f', '\v'} # space, tab, new line, form feed
  KwStart* = Kauto
  KwEnd* = K_Thread_local

var
  gkeywordtable* = initTable[string, Token](int(KwEnd) - int(KwStart) + 1)

proc isKeyword*(a: string): Token =
  gkeywordtable.getOrDefault(a, TNul)

for k in int(KwStart)..int(KwEnd):
  gkeywordtable[$cast[Token](k)] = cast[Token](k)

const type_specifier_set* = {
    Kchar, Kint, Kshort, Ksigned, 
    Kunsigned, 
    Klong, Kdouble, Kfloat,
    K_Atomic,
    K_Complex, Kvoid, K_Bool
} ## primitive types

const function_specifier_set* = {
    Kinline, K_Noreturn
} ## function specfiers

const storage_class_specifier_set* = {
    Ktypedef, Kextern, Kstatic, 
    K_Thread_local, Kauto, Kregister
} ## storage specfiers

const type_qualifier_set* = {
     Kvolatile, Krestrict, Kconst
} ## type qualifiers

const declaration_specifier_set* = 
    type_specifier_set +
    storage_class_specifier_set +
    type_qualifier_set +
    function_specifier_set ## declaration specfiers