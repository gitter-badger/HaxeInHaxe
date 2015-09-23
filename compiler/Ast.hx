typedef Pos = {
	pfile : String,
	pmin : Int,
	pmax : Int
};

enum Keyword {
	Function;
	Class;
	Var;
	If;
	Else;
	While;
	Do;
	For;
	Break;
	Continue;
	Return;
	Extends;
	Implements;
	Import;
	Switch;
	Case;
	Default;
	Static;
	Public;
	Private;
	Try;
	Catch;
	New;
	This;
	Throw;
	Extern;
	Enum;
	In;
	Interface;
	Untyped;
	Cast;
	Override;
	Typedef;
	Dynamic;
	Package;
	Inline;
	Using;
	Null;
	True;
	False;
	Abstract;
	Macro;
};

enum Binop {
	OpAdd;
	OpMult;
	OpDiv;
	OpSub;
	OpAssign;
	OpEq;
	OpNotEq;
	OpGt;
	OpGte;
	OpLt;
	OpLte;
	OpAnd;
	OpOr;
	OpXor;
	OpBoolAnd;
	OpBoolOr;
	OpShl;
	OpShr;
	OpUShr;
	OpMod;
	OpAssignOp(value: Binop);
	OpInterval;
	OpArrow;
};

enum Unop {
	Increment;
	Decrement;
	Not;
	Neg;
	NegBits;
};

enum Constant {
	Int(value: String);
	Float(value: String);
	String(value: String);
	Ident(value: String);
	Regexp(value: StringString);
};

enum Token {
	Eof;
	Const(value: Constant);
	Kwd(value: Keyword);
	Comment(value: String);
	CommentLine(value: String);
	Binop(value: Binop);
	Unop(value: Unop);
	Semicolon;
	Comma;
	BrOpen;
	BrClose;
	BkOpen;
	BkClose;
	POpen;
	PClose;
	Dot;
	DblDot;
	Arrow;
	IntInterval(value: String);
	Sharp(value: String);
	Question;
	At;
	Dollar(value: String);
};

enum Unop_flag {
	Prefix;
	Postfix;
};

enum While_flag {
	NormalWhile;
	DoWhile;
};

typedef Type_path = {
	tpackage : List<String>,
	tname : String,
	tparams : List<Type_param_or_const>,
	tsub : Option<String>
};

enum Type_param_or_const {
	TPType(value: Complex_type);
	TPExpr(value: Expr);
};

enum Complex_type {
	CTPath(value: Type_path);
	CTFunction(value: List<Complex_type>Complex_type);
	CTAnonymous(value: List<Class_field>);
	CTParent(value: Complex_type);
	CTExtend(value: List<Type_path>List<Class_field>);
	CTOptional(value: Complex_type);
};

typedef Func = {
	f_params : List<Type_param>,
	f_args : List<Tuple<String, Bool, Option<Complex_type>, Option<Expr>>>,
	f_type : Option<Complex_type>,
	f_expr : Option<Expr>
};

enum Expr_def {
	EConst(value: Constant);
	EArray(value: ExprExpr);
	EBinop(value: BinopExprExpr);
	EField(value: ExprString);
	EParenthesis(value: Expr);
	EObjectDecl(value: List<Tuple<String, Expr>>);
	EArrayDecl(value: List<Expr>);
	ECall(value: ExprList<Expr>);
	ENew(value: Type_pathList<Expr>);
	EUnop(value: UnopUnop_flagExpr);
	EVars(value: List<Tuple<String, Option<Complex_type>, Option<Expr>>>);
	EFunction(value: Option<String>Func);
	EBlock(value: List<Expr>);
	EFor(value: ExprExpr);
	EIn(value: ExprExpr);
	EIf(value: ExprExprOption<Expr>);
	EWhile(value: ExprExprWhile_flag);
	ESwitch(value: ExprList<Tuple<List<Expr>, Option<Expr>, Option<Expr>>>Option<Expr>);
	ETry(value: ExprList<Tuple<String, Complex_type, Expr>>);
	EReturn(value: Option<Expr>);
	EBreak;
	EContinue;
	EUntyped(value: Expr);
	EThrow(value: Expr);
	ECast(value: ExprOption<Complex_type>);
	EDisplay(value: ExprBool);
	EDisplayNew(value: Type_path);
	ETernary(value: ExprExprExpr);
	ECheckType(value: ExprComplex_type);
	EMeta(value: Metadata_entryExpr);
};

typedef Expr = Tuple2<Expr_def, Pos>;

typedef Type_param = {
	tp_name : String,
	tp_params : List<Type_param>,
	tp_constraints : List<Complex_type>,
	tp_meta : Metadata
};

typedef Documentation = Option<String>;

typedef Metadata_entry = Tuple<Meta.Strict_meta, List<Expr>, Pos>;

typedef Metadata = List<Metadata_entry>;

enum Access {
	APublic;
	APrivate;
	AStatic;
	AOverride;
	ADynamic;
	AInline;
	AMacro;
};

enum Class_field_kind {
	FVar(value: Option<Complex_type>Option<Expr>);
	FFun(value: Func);
	FProp(value: StringStringOption<Complex_type>Option<Expr>);
};

typedef Class_field = {
	cff_name : String,
	cff_doc : Documentation,
	cff_pos : Pos,
	cff_meta : Metadata,
	cff_access : List<Access>,
	cff_kind : Class_field_kind
};

enum Enum_flag {
	EPrivate;
	EExtern;
};

enum Class_flag {
	HInterface;
	HExtern;
	HPrivate;
	HExtends(value: Type_path);
	HImplements(value: Type_path);
};

enum Abstract_flag {
	APrivAbstract;
	AFromType(value: Complex_type);
	AToType(value: Complex_type);
	AIsType(value: Complex_type);
};

typedef Enum_constructor = {
	ec_name : String,
	ec_doc : Documentation,
	ec_meta : Metadata,
	ec_args : List<Tuple<String, Bool, Complex_type>>,
	ec_pos : Pos,
	ec_params : List<Type_param>,
	ec_type : Option<Complex_type>
};

typedef Definition = {
	d_name : String,
	d_doc : Documentation,
	d_params : List<Type_param>,
	d_meta : Metadata,
	d_flags : List<TyQuo(a)>,
	d_data : TyQuo(b)
};

enum Import_mode {
	INormal;
	IAsName(value: String);
	IAll;
};

typedef Import = Tuple<List<Tuple<String, Pos>>, Import_mode>;

enum Type_def {
	EClass(value: Definition<Class_flag, Class_field>);
	EEnum(value: Definition<Enum_flag, Enum_constructor>);
	ETypedef(value: Definition<Enum_flag, Complex_type>);
	EAbstract(value: Definition<Abstract_flag, Class_field>);
	EImport(value: Import);
	EUsing(value: Type_path);
};

typedef Type_decl = Tuple2<Type_def, Pos>;

typedef Package = Tuple<List<String>, List<Type_decl>>;

class /*exception*/ Error {

};

class Ast {
	public static function is_lower_ident(i) return {
		function loop(p) return {
			switch (String.unsafe_get(i, p)) {
			case 'a' .. 'z': True;
			case '_': if ( < (+(p, 1), String.length(i))) {
					loop(+(p, 1));
				} else {
					True;
				};
			case _: False;
			};
		};
		loop(0);
	};

	public static var pos = snd;

	public static function is_postfix(Tuple(e, _), op) return {
		switch (op) {
		case Increment | Decrement: True;
		case Not | Neg | NegBits: False;
		};
	};

	public static function is_prefix(match) return switch (match) {
	case Increment | Decrement: True;
	case Not | Neg | NegBits: True;
	};

	public static var base_class_name = snd;

	public static var null_pos = { () with pfile = "?";
								   pmin = -1;
								   pmax = -1
								 };

	public static function punion(p, p2) return {
		{
			() with pfile = p.pfile;
			pmin = min(p.pmin, p2.pmin);
			pmax = max(p.pmax, p2.pmax)
		};
	};

	public static function punion_el(el) return {
		switch (el) {
		case []: null_pos;
		case ::((_, p), []): p;
		case ::((_, p), el): punion(p, punion_el(el));
		};
	};

	public static function s_type_path(Tuple(p, s)) return {
		switch (p) {
		case []: s;
		case _: ^ (String.concat(".", p), ^ (".", s));
		};
	};

	public static function parse_path(s) return {
		switch (List.rev(ExtString.String.nsplit(s, "."))) {
		case []: failwith("Invalid empty path");
		case ::(x, l): (new Tuple(List.rev(l), x));
		};
	};

	public static function s_escape( ? : (hex = True), s) return {
		var b = Buffer.create(String.length(s));
		for (i in /*to*/0... - (String.length(s), 1)) {
			switch (si) {
			case '\n': Buffer.add_string(b, "\\n");
			case '\t': Buffer.add_string(b, "\\t");
			case '\r': Buffer.add_string(b, "\\r");
			case '"': Buffer.add_string(b, "\\\"");
			case '\\': Buffer.add_string(b, "\\\\");
			case c if (&&(<(int_of_char(c), 32), hex)): Buffer.add_string(b, Printf.sprintf("\\x%.2X", int_of_char(c)));
			case c: Buffer.add_char(b, c);
			};
		};
		Buffer.contents(b);
	};

	public static function s_constant(match) return switch (match) {
	case Int(s): s;
	case Float(s): s;
	case String(s): ^ ("\"", ^ (s_escape(s), "\""));
	case Ident(s): s;
	case Regexp(r, o): ^ ("~/", ^ (r, "/"));
	};

	public static function s_access(match) return switch (match) {
	case APublic: "public";
	case APrivate: "private";
	case AStatic: "static";
	case AOverride: "override";
	case ADynamic: "dynamic";
	case AInline: "inline";
	case AMacro: "macro";
	};

	public static function s_keyword(match) return switch (match) {
	case Function: "function";
	case Class: "class";
	case Static: "static";
	case Var: "var";
	case If: "if";
	case Else: "else";
	case While: "while";
	case Do: "do";
	case For: "for";
	case Break: "break";
	case Return: "return";
	case Continue: "continue";
	case Extends: "extends";
	case Implements: "implements";
	case Import: "import";
	case Switch: "switch";
	case Case: "case";
	case Default: "default";
	case Private: "private";
	case Public: "public";
	case Try: "try";
	case Catch: "catch";
	case New: "new";
	case This: "this";
	case Throw: "throw";
	case Extern: "extern";
	case Enum: "enum";
	case In: "in";
	case Interface: "interface";
	case Untyped: "untyped";
	case Cast: "cast";
	case Override: "override";
	case Typedef: "typedef";
	case Dynamic: "dynamic";
	case Package: "package";
	case Inline: "inline";
	case Using: "using";
	case Null: "null";
	case  True: "true";
	case  False: "false";
	case Abstract: "abstract";
	case Macro: "macro";
	};

	public static function s_binop(match) return switch (match) {
	case OpAdd: "+";
	case OpMult: "*";
	case OpDiv: "/";
	case OpSub: "-";
	case OpAssign: "=";
	case OpEq: "==";
	case OpNotEq: "!=";
	case OpGte: ">=";
	case OpLte: "<=";
	case OpGt: ">";
	case OpLt: "<";
	case OpAnd: "&";
	case OpOr: "|";
	case OpXor: "^";
	case OpBoolAnd: "&&";
	case OpBoolOr: "||";
	case OpShr: "> >";
	case OpUShr: "> > >";
	case OpShl: "< <";
	case OpMod: "%";
	case OpAssignOp(op): ^ (s_binop(op), "=");
	case OpInterval: "...";
	case OpArrow: "=>";
	};

	public static function s_unop(match) return switch (match) {
	case Increment: "++";
	case Decrement: "--";
	case Not: "!";
	case Neg: "-";
	case NegBits: "~";
	};

	public static function s_token(match) return switch (match) {
	case Eof: "<end of file>";
	case Const(c): s_constant(c);
	case Kwd(k): s_keyword(k);
	case Comment(s): ^ ("/*", ^ (s, "*/"));
	case CommentLine(s): ^ ("//", s);
	case Binop(o): s_binop(o);
	case Unop(o): s_unop(o);
	case Semicolon: ";";
	case Comma: ",";
	case BkOpen: "[";
	case BkClose: "]";
	case BrOpen: "{";
	case BrClose: "}";
	case POpen: "[";
	case PClose: "]";
	case Dot: ".";
	case DblDot: ":";
	case Arrow: "->";
	case IntInterval(s): ^ (s, "...");
	case Sharp(s): ^ ("#", s);
	case Question: "?";
	case At: "@";
	case Dollar(v): ^ ("$", v);
	};

	public static function unescape(s) return {
		var b = Buffer.create(0);
		function loop(esc, i) return {
			if ( = (i, String.length(s))) {
				[];
			} else {
				var c = si;
				if (esc) {
					var inext = ref(+(i, 1));
					switch (c) {
					case 'n': Buffer.add_char(b, '\n');
					case 'r': Buffer.add_char(b, '\r');
					case 't': Buffer.add_char(b, '\t');
					case '"' | '\'' | '\\': Buffer.add_char(b, c);
					case '0' .. '3': var c = try {
							char_of_int(int_of_string( ^ ("0o", String.sub(s, i, 3))));
						} catch (e: _) {
							raise(Exit);
						};
						Buffer.add_char(b, c);
						inext.val = +(inext.val, 2);
					case 'x': var c = try {
							char_of_int(int_of_string( ^ ("0x", String.sub(s, +(i, 1), 2))));
						} catch (e: _) {
							raise(Exit);
						};
						Buffer.add_char(b, c);
						inext.val = +(inext.val, 2);
					case 'u': var Tuple(u, a) = try {
							(new Tuple(int_of_string( ^ ("0x", String.sub(s, +(i, 1), 4))), 4));
						} catch (e: _) {
							try {
								assert = (s + (i, 1), '{');
								var l = -(String.index_from(s, +(i, 3), '}'), +(i, 2));
								var u = int_of_string( ^ ("0x", String.sub(s, +(i, 2), l)));
								assert <= (u, 0x10FFFF);
								(new Tuple(u, +(l, 2)));
							} catch (e: _) {
								raise(Exit);
							};
						};
						var ub = UTF8.Buf.create(0);
						UTF8.Buf.add_char(ub, UChar.uchar_of_int(u));
						Buffer.add_string(b, UTF8.Buf.contents(ub));
						inext.val = +(inext.val, a);
					case _: raise(Exit);
					};
					loop(False, inext.val);
				} else {
					switch (c) {
					case '\\': loop(True, +(i, 1));
					case c: Buffer.add_char(b, c);
						loop(False, +(i, 1));
					};
				};
			};
		};
		loop(False, 0);
		Buffer.contents(b);
	};

	public static function map_expr(loop, Tuple(e, p)) return {
		function opt(f, o) return {
			switch (o) {
			case None: None;
			case Some(v): Some(f(v));
			};
		};
		function tparam(match) return switch (match) {
		case TPType(t): TPType(ctype(t));
		case TPExpr(e): TPExpr(loop(e));
		};
		function cfield(f) return {
			{
				(f) with cff_kind = switch (f.cff_kind) {
				case FVar(t, e): FVar(opt(ctype, t), opt(loop, e));
				case FFun(f): FFun(func(f));
				case FProp(get, set, t, e): FProp(get, set, opt(ctype, t), opt(loop, e));
				}
			};
		};
		function ctype(match) return switch (match) {
		case CTPath(t): CTPath(tpath(t));
		case CTFunction(cl, c): CTFunction(List.map(ctype, cl), ctype(c));
		case CTAnonymous(fl): CTAnonymous(List.map(cfield, fl));
		case CTParent(t): CTParent(ctype(t));
		case CTExtend(tl, fl): CTExtend(List.map(tpath, tl), List.map(cfield, fl));
		case CTOptional(t): CTOptional(ctype(t));
		};
		function tparamdecl(t) return {
			{
				() with tp_name = t.tp_name;
				tp_constraints = List.map(ctype, t.tp_constraints);
				tp_params = List.map(tparamdecl, t.tp_params);
				tp_meta = t.tp_meta
			};
		};
		function func(f) return {
			{
				() with f_params = List.map(tparamdecl, f.f_params);
				f_args = List.map(function (n, o, t, e): (new Tuple(n, o, opt(ctype, t), opt(loop, e))), f.f_args);
				f_type = opt(ctype, f.f_type);
				f_expr = opt(loop, f.f_expr)
			};
		};
		function tpath(t) return {
			{ (t) with tparams = List.map(tparam, t.tparams) };
		};
		var e = switch (e) {
		case EConst(_): e;
		case EArray(e1, e2): EArray(loop(e1), loop(e2));
		case EBinop(op, e1, e2): EBinop(op, loop(e1), loop(e2));
		case EField(e, f): EField(loop(e), f);
		case EParenthesis(e): EParenthesis(loop(e));
		case EObjectDecl(fl): EObjectDecl(List.map(function (f, e): (new Tuple(f, loop(e))), fl));
		case EArrayDecl(el): EArrayDecl(List.map(loop, el));
		case ECall(e, el): ECall(loop(e), List.map(loop, el));
		case ENew(t, el): ENew(tpath(t), List.map(loop, el));
		case EUnop(op, f, e): EUnop(op, f, loop(e));
		case EVars(vl): EVars(List.map(function (n, t, eo): (new Tuple(n, opt(ctype, t), opt(loop, eo))), vl));
		case EFunction(n, f): EFunction(n, func(f));
		case EBlock(el): EBlock(List.map(loop, el));
		case EFor(e1, e2): EFor(loop(e1), loop(e2));
		case EIn(e1, e2): EIn(loop(e1), loop(e2));
		case EIf(e, e1, e2): EIf(loop(e), loop(e1), opt(loop, e2));
		case EWhile(econd, e, f): EWhile(loop(econd), loop(e), f);
		case ESwitch(e, cases, def): ESwitch(loop(e), List.map(function (el, eg, e): (new Tuple(List.map(loop, el), opt(loop, eg),
												 opt(loop, e))), cases), opt(opt(loop), def));
		case ETry(e, catches): ETry(loop(e), List.map(function (n, t, e): (new Tuple(n, ctype(t), loop(e))), catches));
		case EReturn(e): EReturn(opt(loop, e));
		case EBreak: EBreak;
		case EContinue: EContinue;
		case EUntyped(e): EUntyped(loop(e));
		case EThrow(e): EThrow(loop(e));
		case ECast(e, t): ECast(loop(e), opt(ctype, t));
		case EDisplay(e, f): EDisplay(loop(e), f);
		case EDisplayNew(t): EDisplayNew(tpath(t));
		case ETernary(e1, e2, e3): ETernary(loop(e1), loop(e2), loop(e3));
		case ECheckType(e, t): ECheckType(loop(e), ctype(t));
		case EMeta(m, e): EMeta(m, loop(e));
		};
		(new Tuple(e, p));
	};

	public static function s_expr(e) return {
		function s_expr_inner(tabs, Tuple(e, _)) return {
			switch (e) {
			case EConst(c): s_constant(c);
			case EArray(e1, e2): ^ (s_expr_inner(tabs, e1), ^ ("[", ^ (s_expr_inner(tabs, e2), "]")));
			case EBinop(op, e1, e2): ^ (s_expr_inner(tabs, e1), ^ (" ", ^ (s_binop(op), ^ (" ", s_expr_inner(tabs, e2)))));
			case EField(e, f): ^ (s_expr_inner(tabs, e), ^ (".", f));
			case EParenthesis(e): ^ ("[", ^ (s_expr_inner(tabs, e), "]"));
			case EObjectDecl(fl): ^ ("{ ", ^ (String.concat(", ", List.map(function (n, e): ^ (n, ^ (" : ", s_expr_inner(tabs, e))),
				fl)), " }"));
			case EArrayDecl(el): ^ ("[", ^ (s_expr_list(tabs, el, ", "), "]"));
			case ECall(e, el): ^ (s_expr_inner(tabs, e), ^ ("[", ^ (s_expr_list(tabs, el, ", "), "]")));
			case ENew(t, el): ^ ("new ", ^ (s_complex_type_path(tabs, t), ^ ("[", ^ (s_expr_list(tabs, el, ", "), "]"))));
			case EUnop(op, Postfix, e): ^ (s_expr_inner(tabs, e), s_unop(op));
			case EUnop(op, Prefix, e): ^ (s_unop(op), s_expr_inner(tabs, e));
			case EFunction(Some(n), f): ^ ("function ", ^ (n, s_func(tabs, f)));
			case EFunction(None, f): ^ ("function", s_func(tabs, f));
			case EVars(vl): ^ ("var ", String.concat(", ", List.map(s_var(tabs), vl)));
			case EBlock([]): "{ }";
			case EBlock(el): s_block(tabs, el, "{", "\n", "}");
			case EFor(e1, e2): ^ ("for [", ^ (s_expr_inner(tabs, e1), ^ ("] ", s_expr_inner(tabs, e2))));
			case EIn(e1, e2): ^ (s_expr_inner(tabs, e1), ^ (" in ", s_expr_inner(tabs, e2)));
			case EIf(e, e1, None): ^ ("if [", ^ (s_expr_inner(tabs, e), ^ ("] ", s_expr_inner(tabs, e1))));
			case EIf(e, e1, Some(e2)): ^ ("if [", ^ (s_expr_inner(tabs, e), ^ ("] ", ^ (s_expr_inner(tabs, e1), ^ (" else ",
				s_expr_inner(tabs, e2))))));
			case EWhile(econd, e, NormalWhile): ^ ("while [", ^ (s_expr_inner(tabs, econd), ^ ("] ", s_expr_inner(tabs, e))));
			case EWhile(econd, e, DoWhile): ^ ("do ", ^ (s_expr_inner(tabs, e), ^ (" while [", ^ (s_expr_inner(tabs, econd), "]"))));
			case ESwitch(e, cases, def): ^ ("switch ", ^ (s_expr_inner(tabs, e), ^ (" {\n\t", ^ (tabs, ^ (String.concat( ^ ("\n\t",
				tabs), List.map(s_case(tabs), cases)), ^ (switch (def) {
			case None: "";
			case Some(def): ^ ("\n\t", ^ (tabs, ^ ("default:", switch (def) {
				case None: "";
				case Some(def): s_expr_omit_block(tabs, def);
					})));
				}, ^ ("\n", ^ (tabs, "}"))))))));
			case ETry(e, catches): ^ ("try ", ^ (s_expr_inner(tabs, e), String.concat("", List.map(s_catch(tabs), catches))));
			case EReturn(e): ^ ("return", s_opt_expr(tabs, e, " "));
			case EBreak: "break";
			case EContinue: "continue";
			case EUntyped(e): ^ ("untyped ", s_expr_inner(tabs, e));
			case EThrow(e): ^ ("throw ", s_expr_inner(tabs, e));
			case ECast(e, Some(t)): ^ ("cast [", ^ (s_expr_inner(tabs, e), ^ (", ", ^ (s_complex_type(tabs, t), "]"))));
			case ECast(e, None): ^ ("cast ", s_expr_inner(tabs, e));
			case ETernary(e1, e2, e3): ^ (s_expr_inner(tabs, e1), ^ (" ? ", ^ (s_expr_inner(tabs, e2), ^ (" : ", s_expr_inner(tabs,
											  e3)))));
			case ECheckType(e, t): ^ ("[", ^ (s_expr_inner(tabs, e), ^ (" : ", ^ (s_complex_type(tabs, t), "]"))));
			case EMeta(m, e): ^ (s_metadata(tabs, m), ^ (" ", s_expr_inner(tabs, e)));
			case _: "";
			};
		};
		function s_expr_list(tabs, el, sep) return {
			String.concat(sep, List.map(s_expr_inner(tabs), el));
		};
		function s_complex_type_path(tabs, t) return {
			^ (String.concat(".", t.tpackage), if ( > (List.length(t.tpackage), 0)) {
			".";
		} else {
			^ ("", ^ (t.tname, switch (t.tsub) {
			case Some(s): ^ (".", s);
				case None: ^ ("", s_type_param_or_consts(tabs, t.tparams));
				}));
			});
		};
		function s_type_param_or_consts(tabs, pl) return {
			if ( > (List.length(pl), 0)) {
				^ ("<", ^ (String.concat(",", List.map(s_type_param_or_const(tabs), pl)), ">"));
			} else {
				"";
			};
		};
		function s_type_param_or_const(tabs, p) return {
			switch (p) {
			case TPType(t): s_complex_type(tabs, t);
			case TPExpr(e): s_expr_inner(tabs, e);
			};
		};
		function s_complex_type(tabs, ct) return {
			switch (ct) {
			case CTPath(t): s_complex_type_path(tabs, t);
			case CTFunction(cl, c): if ( > (List.length(cl), 0)) {
					String.concat(" -> ", List.map(s_complex_type(tabs), cl));
				} else {
					^ ("Void", ^ (" -> ", s_complex_type(tabs, c)));
				};
			case CTAnonymous(fl): ^ ("{ ", ^ (String.concat("; ", List.map(s_class_field(tabs), fl)), "}"));
			case CTParent(t): ^ ("[", ^ (s_complex_type(tabs, t), "]"));
			case CTOptional(t): ^ ("?", s_complex_type(tabs, t));
			case CTExtend(tl, fl): ^ ("{> ", ^ (String.concat(" >, ", List.map(s_complex_type_path(tabs), tl)), ^ (", ",
													^ (String.concat(", ", List.map(s_class_field(tabs), fl)), " }"))));
			};
		};
		function s_class_field(tabs, f) return {
			switch (f.cff_doc) {
			case Some(s): ^ ("/**\n\t", ^ (tabs, ^ (s, "\n**/\n")));
			case None: ^ ("", if ( > (List.length(f.cff_meta), 0)) {
				String.concat( ^ ("\n", tabs), List.map(s_metadata(tabs), f.cff_meta));
				} else {
					^ ("", if ( > (List.length(f.cff_access), 0)) {
					String.concat(" ", List.map(s_access, f.cff_access));
					} else {
						^ ("", switch (f.cff_kind) {
					case FVar(t, e): ^ ("var ", ^ (f.cff_name, ^ (s_opt_complex_type(tabs, t, " : "), s_opt_expr(tabs, e, " = "))));
						case FProp(get, set, t, e): ^ ("var ", ^ (f.cff_name, ^ ("[", ^ (get, ^ (",", ^ (set, ^ ("]", ^ (s_opt_complex_type(tabs,
														   t, " : "), s_opt_expr(tabs, e, " = ")))))))));
						case FFun(func): ^ ("function ", ^ (f.cff_name, s_func(tabs, func)));
						});
					});
				});
			};
		};
		function s_metadata(tabs, Tuple(s, e, _)) return {
			^ ("@", ^ (Meta.to_string(s), if ( > (List.length(e), 0)) {
			^ ("[", ^ (s_expr_list(tabs, e, ", "), "]"));
			} else {
				"";
			}));
		};
		function s_opt_complex_type(tabs, t, pre) return {
			switch (t) {
			case Some(s): ^ (pre, s_complex_type(tabs, s));
			case None: "";
			};
		};
		function s_opt_expr(tabs, e, pre) return {
			switch (e) {
			case Some(s): ^ (pre, s_expr_inner(tabs, s));
			case None: "";
			};
		};
		function s_func(tabs, f) return {
			^ (s_type_param_list(tabs, f.f_params), ^ ("[", ^ (String.concat(", ", List.map(s_func_arg(tabs), f.f_args)), ^ ("]", ^ (s_opt_complex_type(tabs, f.f_type, ":"), s_opt_expr(tabs, f.f_expr, " "))))));
		};
		function s_type_param(tabs, t) return {
			^ (t.tp_name, ^ (s_type_param_list(tabs, t.tp_params), if ( > (List.length(t.tp_constraints), 0)) {
			^ (":[", ^ (String.concat(", ", List.map(s_complex_type(tabs), t.tp_constraints)), "]"));
			} else {
				"";
			}));
		};
		function s_type_param_list(tabs, tl) return {
			if ( > (List.length(tl), 0)) {
				^ ("<", ^ (String.concat(", ", List.map(s_type_param(tabs), tl)), ">"));
			} else {
				"";
			};
		};
		function s_func_arg(tabs, Tuple(n, o, t, e)) return {
			if (o) {
				"?";
			} else {
				^ ("", ^ (n, ^ (s_opt_complex_type(tabs, t, ":"), s_opt_expr(tabs, e, " = "))));
			};
		};
		function s_var(tabs, Tuple(n, t, e)) return {
			^ (n, ^ (s_opt_complex_type(tabs, t, ":"), s_opt_expr(tabs, e, " = ")));
		};
		function s_case(tabs, Tuple(el, e1, e2)) return {
			^ ("case ", ^ (s_expr_list(tabs, el, ", "), ^ (switch (e1) {
		case None: ":";
		case Some(e): ^ (" if [", ^ (s_expr_inner(tabs, e), "]:"));
			}, switch (e2) {
		case None: "";
		case Some(e): s_expr_omit_block(tabs, e);
			})));
		};
		function s_catch(tabs, Tuple(n, t, e)) return {
			^ (" catch[", ^ (n, ^ (":", ^ (s_complex_type(tabs, t), ^ ("] ", s_expr_inner(tabs, e))))));
		};
		function s_block(tabs, el, opn, nl, cls) return {
			^ (opn, ^ ("\n\t", ^ (tabs, ^ (s_expr_list( ^ (tabs, "\t"), el, ^ (";\n\t", tabs)), ^ (";", ^ (nl, ^ (tabs, cls)))))));
		};
		function s_expr_omit_block(tabs, e) return {
			switch (e) {
			case (EBlock([]), _): "";
			case (EBlock(el), _): s_block( ^ (tabs, "\t"), el, "", "", "");
			case _: ^ (s_expr_inner( ^ (tabs, "\t"), e), ";");
			};
		};
		s_expr_inner("", e);
	};

	public static function get_value_meta(meta) return {
		try {
			switch (Meta.get(Meta.Value, meta)) {
			case (_, ::((EObjectDecl(values), _), []), _): List.fold_left(function acc: function (s, e): PMap.add(s, e, acc),
				PMap.empty, values);
			case _: raise(Not_found);
			};
		} catch (e: Not_found) {
			PMap.empty;
		};
	};

	public static function string_list_of_expr_path_raise(Tuple(e, p)) return {
		switch (e) {
		case EConst(Ident(i)): ::(i, []);
		case EField(e, f): ::(f, string_list_of_expr_path_raise(e));
		case _: raise(Exit);
		};
	};

	public static function expr_of_type_path(Tuple(sl, s), p) return {
		switch (sl) {
		case []: (new Tuple(EConst(Ident(s)), p));
		case ::(s1, sl): var e1 = (new Tuple(EConst(Ident(s1)), p));
			var e = List.fold_left(function e: function s: (new Tuple(EField(e, s), p)), e1, sl);
			(new Tuple(EField(e, s), p));
		};
	};

	public static function match_path(recursive, sl, sl_pattern) return {
		function loop(sl1, sl2) return {
			switch ((new Tuple(sl1, sl2))) {
			case ([], []): True;
			case (::(s1, ::(s11, _)), ::(s2, [])) if (&&(is_lower_ident(s2), !(is_lower_ident(s11)))): = (s1, s2);
			case (::(_, []), ::(, [])): True;
			case (_, [] | ::(, [])): recursive;
			case ([], _): False;
			case (::(s1, sl1), ::(s2, sl2)): && ( = (s1, s2), loop(sl1, sl2));
			};
		};
		loop(sl, sl_pattern);
	};

	public static function full_dot_path(mpath, tpath) return {
		if ( = (mpath, tpath)) {
			@(fst(tpath), ::(snd(tpath), []));
		} else {
			@(fst(mpath), ::(snd(mpath), ::(snd(tpath), [])));
		};
	}
}
