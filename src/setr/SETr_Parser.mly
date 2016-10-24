%token LB RB EOF TRUE FALSE HELP
%token <string> IDENT
%token <string> INT
%token <string> STRING

%start construct
%type <SETr_DomainRegistrar.t> construct

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
  | TRUE { SETr_DomainRegistrar.Bool true }
  | FALSE { SETr_DomainRegistrar.Bool false }
  | STRING { SETr_DomainRegistrar.String $1 }
  | INT { SETr_DomainRegistrar.Int (int_of_string $1) }
  ;
