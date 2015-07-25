%token LB RB EOF TRUE FALSE HELP
%token <string> IDENT
%token <string> INT
%token <string> STRING

%start construct
%type <SETr_DomainBuilder.t> construct

%%

construct
  : domain EOF { $1 }
  | HELP EOF { SETr_DomainBuilder.get_help () }
  ;

domain
  : IDENT arg_list { SETr_DomainBuilder.build_domain $1 $2 }
  ;

arg_list
  :  { [] }
  | LB args RB { $2 }
  ;

args
  : arg      { [$1] }
  | arg args { $1::$2 }
  ;

arg
  : domain { $1 }
  | TRUE { SETr_DomainBuilder.Bool true }
  | FALSE { SETr_DomainBuilder.Bool false }
  | STRING { SETr_DomainBuilder.String $1 }
  | INT { SETr_DomainBuilder.Int (int_of_string $1) }
  ;