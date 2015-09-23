import Ast;

enum Error_msg {
	Unexpected(value: Token);
	Duplicate_default;
	Missing_semicolon;
	Unclosed_macro;
	Unimplemented;
	Missing_type;
	Custom(value: String);
};

class /*exception*/ Error {

};

class /*exception*/ TypePath {

};

class /*exception*/ Display {

};

enum Small_type {
	TNull;
	TBool(value: Bool);
	TFloat(value: Float);
	TString(value: String);
};

class Parser {
	public static function error_msg(match) return switch (match) {
	case Unexpected(t): ^ ("Unexpected ", s_token(t));
	case Duplicate_default: "Duplicate default";
	case Missing_semicolon: "Missing ;";
	case Unclosed_macro: "Unclosed macro";
	case Unimplemented: "Not implemented for current platform";
	case Missing_type: "Missing type declaration";
	case Custom(s): s;
	};

	public static function error(m, p) return {
		raise(Error(m, p));
	};

	public static var display_error = ref(function _: function _: assert False) : ref(error_msg -> pos -> unit);

	public static var quoted_ident_prefix = "@$__hx__";

	public static function quote_ident(s) return {
		^ (quoted_ident_prefix, s);
	};

	public static function unquote_ident(f) return {
		var pf = quoted_ident_prefix;
		var pflen = String.length(pf);
		var is_quoted = && ( >= (String.length(f), pflen), = (String.sub(f, 0, pflen), pf));
		var s = if (is_quoted) {
			String.sub(f, pflen, -(String.length(f), pflen));
		} else {
			f;
		};
		var is_valid = || (!(is_quoted), try {
			for (i in /*to*/0... - (String.length(s), 1)) {
				switch (String.unsafe_get(s, i)) {
				case 'a' .. 'z' | 'A' .. 'Z' | '_': [];
				case '0' .. '9' if (>(i, 0)): [];
				case _: raise(Exit);
				};
			};
			if (Hashtbl.mem(Lexer.keywords, s)) {
				raise(Exit);
			} else {
				[];
			};
			True;
		} catch (e: Exit) {
			False;
		});
		(new Tuple(s, is_quoted, is_valid));
	};

	public static var cache = ref(DynArray.create([]));

	public static var last_doc = ref(None);

	public static var use_doc = ref(False);

	public static var use_parser_resume = ref(True);

	public static var resume_display = ref(null_pos);

	public static var in_macro = ref(False);

	public static function last_token(s) return {
		var n = Stream.count(s);
		DynArray.get(cache.val, if ( = (n, 0)) {
		0;
	} else {
		-(n, 1);
		});
	};

	public static function serror([]) return {
		raise(Stream.Error(""));
	};

	public static function do_resume([]) return {
		<>(resume_display.val, null_pos);
	};

	public static function display(e) return {
		raise(Display(e));
	};

	public static function type_path(sl, in_import) return {
		switch (sl) {
		case ::(n, l) if (&&(>=(n0, 'A'), <=(n0, 'Z'))): raise(TypePath(List.rev(l), Some(n, False), in_import));
		case _: raise(TypePath(List.rev(sl), None, in_import));
		};
	};

	public static function is_resuming(p) return {
		var p2 = resume_display.val;
		&& ( = (p.pmax, p2.pmin), && (use_parser_resume.val, = (Common.unique_full_path(p.pfile), p2.pfile)));
	};

	public static function set_resume(p) return {
		resume_display.val = { (p) with pfile = Common.unique_full_path(p.pfile) };
	};

	public static function is_dollar_ident(e) return {
		switch (fst(e)) {
		case EConst(Ident(n)) if (=(n0, '$')): True;
		case _: False;
		};
	};

	public static function precedence(op) return {
		var left = True;
		var right = False;
		switch (op) {
		case OpMod: (new Tuple(0, left));
		case OpMult | OpDiv: (new Tuple(1, left));
		case OpAdd | OpSub: (new Tuple(2, left));
		case OpShl | OpShr | OpUShr: (new Tuple(3, left));
		case OpOr | OpAnd | OpXor: (new Tuple(4, left));
		case OpEq | OpNotEq | OpGt | OpLt | OpGte | OpLte: (new Tuple(5, left));
		case OpInterval: (new Tuple(6, left));
		case OpBoolAnd: (new Tuple(7, left));
		case OpBoolOr: (new Tuple(8, left));
		case OpArrow: (new Tuple(9, right));
		case OpAssign | OpAssignOp(_): (new Tuple(10, right));
		};
	};

	public static function is_not_assign(match) return switch (match) {
	case OpAssign | OpAssignOp(_): False;
	case _: True;
	};

	public static function swap(op1, op2) return {
		var Tuple(p1, left1) = precedence(op1);
		var Tuple(p2, _) = precedence(op2);
		&& (left1, <= (p1, p2));
	};

	public static function make_binop(op, e, Tuple(v, p2) = e2) return {
		switch (v) {
		case EBinop(_op, _e, _e2) if (swap(op, _op)): var _e = make_binop(op, e, _e);
			(new Tuple(EBinop(_op, _e, _e2), punion(pos(_e), pos(_e2))));
		case ETernary(e1, e2, e3) if (is_not_assign(op)): var e = make_binop(op, e, e1);
			(new Tuple(ETernary(e, e2, e3), punion(pos(e), pos(e3))));
		case _: (new Tuple(EBinop(op, e, e2), punion(pos(e), pos(e2))));
		};
	};

	public static function make_unop(op, Tuple(v, p2) = e, p1) return {
		switch (v) {
		case EBinop(bop, e, e2): (new Tuple(EBinop(bop, make_unop(op, e, p1), e2), punion(p1, p2)));
		case ETernary(e1, e2, e3): (new Tuple(ETernary(make_unop(op, e1, p1), e2, e3), punion(p1, p2)));
		case _: (new Tuple(EUnop(op, Prefix, e), punion(p1, p2)));
		};
	};

	public static function make_meta(name, params, Tuple(v, p2) = e, p1) return {
		switch (v) {
		case EBinop(bop, e, e2): (new Tuple(EBinop(bop, make_meta(name, params, e, p1), e2), punion(p1, p2)));
		case ETernary(e1, e2, e3): (new Tuple(ETernary(make_meta(name, params, e1, p1), e2, e3), punion(p1, p2)));
		case _: (new Tuple(EMeta((new Tuple(name, params, p1)), e), punion(p1, p2)));
		};
	};

	public static function make_is(e, t, p) return {
		var e_is = (new Tuple(EField((new Tuple(EConst(Ident("Std")), p)), "is"), p));
		var e2 = expr_of_type_path((new Tuple(t.tpackage, t.tname)), p);
		(new Tuple(ECall(e_is, ::(e, ::(e2, []))), p));
	};

	public static function reify(in_macro) return {
		var cur_pos = ref(None);
		function mk_enum(ename, n, vl, p) return {
			var constr = (new Tuple(EConst(Ident(n)), p));
			switch (vl) {
			case []: constr;
			case _: (new Tuple(ECall(constr, vl), p));
			};
		};
		function to_const(c, p) return {
			function cst(n, v) return {
				mk_enum("Constant", n, ::((new Tuple(EConst(String(v)), p)), []), p);
			};
			switch (c) {
			case Int(i): cst("CInt", i);
			case String(s): cst("CString", s);
			case Float(s): cst("CFloat", s);
			case Ident(s): cst("CIdent", s);
			case Regexp(r, o): mk_enum("Constant", "CRegexp", ::((new Tuple(EConst(String(r)), p)), ::((new Tuple(EConst(String(o)),
										   p)), [])), p);
			};
		};
		function to_binop(o, p) return {
			function op(n) return {
				mk_enum("Binop", n, [], p);
			};
			switch (o) {
			case OpAdd: op("OpAdd");
			case OpMult: op("OpMult");
			case OpDiv: op("OpDiv");
			case OpSub: op("OpSub");
			case OpAssign: op("OpAssign");
			case OpEq: op("OpEq");
			case OpNotEq: op("OpNotEq");
			case OpGt: op("OpGt");
			case OpGte: op("OpGte");
			case OpLt: op("OpLt");
			case OpLte: op("OpLte");
			case OpAnd: op("OpAnd");
			case OpOr: op("OpOr");
			case OpXor: op("OpXor");
			case OpBoolAnd: op("OpBoolAnd");
			case OpBoolOr: op("OpBoolOr");
			case OpShl: op("OpShl");
			case OpShr: op("OpShr");
			case OpUShr: op("OpUShr");
			case OpMod: op("OpMod");
			case OpAssignOp(o): mk_enum("Binop", "OpAssignOp", ::(to_binop(o, p), []), p);
			case OpInterval: op("OpInterval");
			case OpArrow: op("OpArrow");
			};
		};
		function to_string(s, p) return {
			var len = String.length(s);
			if ( && ( > (len, 1), = (s0, '$'))) {
				(new Tuple(EConst(Ident(String.sub(s, 1, -(len, 1)))), p));
			} else {
				(new Tuple(EConst(String(s)), p));
			};
		};
		function to_array(f, a, p) return {
			(new Tuple(EArrayDecl(List.map(function s: f(s, p), a)), p));
		};
		function to_null(p) return {
			(new Tuple(EConst(Ident("null")), p));
		};
		function to_opt(f, v, p) return {
			switch (v) {
			case None: to_null(p);
			case Some(v): f(v, p);
			};
		};
		function to_bool(o, p) return {
			(new Tuple(EConst(Ident(if (o) {
			"true";
		} else {
			"false";
		})), p));
		};
		function to_obj(fields, p) return {
			(new Tuple(EObjectDecl(fields), p));
		};
		function to_tparam(t, p) return {
			var Tuple(n, v) = switch (t) {
			case TPType(t): (new Tuple("TPType", to_ctype(t, p)));
			case TPExpr(e): (new Tuple("TPExpr", to_expr(e, p)));
			};
			mk_enum("TypeParam", n, ::(v, []), p);
		};
		function to_tpath(t, p) return {
			var len = String.length(t.tname);
			if ( && ( = (t.tpackage, []), && ( > (len, 1), = (t.tname0, '$')))) {
				(new Tuple(EConst(Ident(String.sub(t.tname, 1, -(len, 1)))), p));
			} else {
				var fields = ::((new Tuple("pack", to_array(to_string, t.tpackage, p))), ::((new Tuple("name", to_string(t.tname, p))), ::((new Tuple("params", to_array(to_tparam, t.tparams, p))), [])));
				to_obj(switch (t.tsub) {
			case None: fields;
			case Some(s): @(fields, ::((new Tuple("sub", to_string(s, p))), []));
				}, p);
			};
		};
		function to_ctype(t, p) return {
			function ct(n, vl) return {
				mk_enum("ComplexType", n, vl, p);
			};
			switch (t) {
			case CTPath({ tpackage = []; tparams = []; tsub = None; tname = n }) if (=(n0, '$')): to_string(n, p);
			case CTPath(t): ct("TPath", ::(to_tpath(t, p), []));
			case CTFunction(args, ret): ct("TFunction", ::(to_array(to_ctype, args, p), ::(to_ctype(ret, p), [])));
			case CTAnonymous(fields): ct("TAnonymous", ::(to_array(to_cfield, fields, p), []));
			case CTParent(t): ct("TParent", ::(to_ctype(t, p), []));
			case CTExtend(tl, fields): ct("TExtend", ::(to_array(to_tpath, tl, p), ::(to_array(to_cfield, fields, p), [])));
			case CTOptional(t): ct("TOptional", ::(to_ctype(t, p), []));
			};
		};
		function to_fun(f, p) return {
			function farg(Tuple(n, o, t, e), p) return {
				var fields = ::((new Tuple("name", to_string(n, p))), ::((new Tuple("opt", to_bool(o, p))), ::((new Tuple("type", to_opt(to_ctype, t, p))), [])));
				to_obj(switch (e) {
			case None: fields;
			case Some(e): @(fields, ::((new Tuple("value", to_expr(e, p))), []));
				}, p);
			};
			function fparam(t, p) return {
				var fields = ::((new Tuple("name", to_string(t.tp_name, p))), ::((new Tuple("constraints", to_array(to_ctype, t.tp_constraints, p))), ::((new Tuple("params", to_array(fparam, t.tp_params, p))), [])));
				to_obj(fields, p);
			};
			var fields = ::((new Tuple("args", to_array(farg, f.f_args, p))), ::((new Tuple("ret", to_opt(to_ctype, f.f_type, p))), ::((new Tuple("expr", to_opt(to_expr, f.f_expr, p))), ::((new Tuple("params", to_array(fparam, f.f_params, p))), []))));
			to_obj(fields, p);
		};
		function to_cfield(f, p) return {
			var p = f.cff_pos;
			function to_access(a, p) return {
				var n = switch (a) {
				case APublic: "APublic";
				case APrivate: "APrivate";
				case AStatic: "AStatic";
				case AOverride: "AOverride";
				case ADynamic: "ADynamic";
				case AInline: "AInline";
				case AMacro: "AMacro";
				};
				mk_enum("Access", n, [], p);
			};
			function to_kind(k) return {
				var Tuple(n, vl) = switch (k) {
				case FVar(ct, e): (new Tuple("FVar", ::(to_opt(to_ctype, ct, p), ::(to_opt(to_expr, e, p), []))));
				case FFun(f): (new Tuple("FFun", ::(to_fun(f, p), [])));
				case FProp(get, set, t, e): (new Tuple("FProp", ::(to_string(get, p), ::(to_string(set, p), ::(to_opt(to_ctype, t, p),
					::(to_opt(to_expr, e, p), []))))));
				};
				mk_enum("FieldType", n, vl, p);
			};
			var fields = ::(Some("name", to_string(f.cff_name, p)), ::(switch (f.cff_doc) {
		case None: None;
		case Some(s): Some("doc", to_string(s, p));
			}, ::(switch (f.cff_access) {
		case []: None;
			case l: Some("access", to_array(to_access, l, p));
			}, ::(Some("kind", to_kind(f.cff_kind)), ::(Some("pos", to_pos(f.cff_pos)), ::(switch (f.cff_meta) {
		case []: None;
			case l: Some("meta", to_meta(f.cff_meta, p));
			}, []))))));
			var fields = List.rev(List.fold_left(function acc: function v:
			switch (v) {
		case None: acc;
		case Some(e): ::(e, acc);
			}, [], fields));
			to_obj(fields, p);
		};
		function to_meta(m, p) return {
			to_array(function (m, el, p): function _: var fields = ::((new Tuple("name", to_string(fst(Common.MetaInfo.to_string(m)), p))), ::((new Tuple("params", to_expr_array(el, p))), ::((new Tuple("pos", to_pos(p))), [])));
			to_obj(fields, p), m, p);
		};
		function to_pos(p) return {
			switch (cur_pos.val) {
			case Some(p): p;
			case None: var file = (new Tuple(EConst(String(p.pfile)), p));
				var pmin = (new Tuple(EConst(Int(string_of_int(p.pmin))), p));
				var pmax = (new Tuple(EConst(Int(string_of_int(p.pmax))), p));
				if (in_macro) {
					(new Tuple(EUntyped(ECall((new Tuple(EConst(Ident("__dollar__mk_pos")), p)), ::(file, ::(pmin, ::(pmax, [])))), p), p));
				} else {
					to_obj(::((new Tuple("file", file)), ::((new Tuple("min", pmin)), ::((new Tuple("max", pmax)), []))), p);
				};
			};
		};
		function to_expr_array(a, p) return {
			switch (a) {
			case ::((EMeta((Meta.Dollar(a), [], _), e1), _), []): switch (fst(e1)) {
				case EArrayDecl(el): to_expr_array(el, p);
				case _: e1;
				};
			case _: to_array(to_expr, a, p);
			};
		};
		function to_expr(e, _) return {
			var p = snd(e);
			function expr(n, vl) return {
				var e = mk_enum("ExprDef", n, vl, p);
				to_obj(::((new Tuple("expr", e)), ::((new Tuple("pos", to_pos(p))), [])), p);
			};
			function loop(e) return {
				to_expr(e, snd(e));
			};
			switch (fst(e)) {
			case EConst(Ident(n)) if (&&(=(n0, '$'), >(String.length(n), 1))): to_string(n, p);
			case EConst(c): expr("EConst", ::(to_const(c, p), []));
			case EArray(e1, e2): expr("EArray", ::(loop(e1), ::(loop(e2), [])));
			case EBinop(op, e1, e2): expr("EBinop", ::(to_binop(op, p), ::(loop(e1), ::(loop(e2), []))));
			case EField(e, s): expr("EField", ::(loop(e), ::(to_string(s, p), [])));
			case EParenthesis(e): expr("EParenthesis", ::(loop(e), []));
			case EObjectDecl(fl): expr("EObjectDecl", ::(to_array(function (f, e): to_obj(::((new Tuple("field", to_string(f, p))),
										   ::((new Tuple("expr", loop(e))), []))), fl, p), []));
			case EArrayDecl(el): expr("EArrayDecl", ::(to_expr_array(el, p), []));
			case ECall(e, el): expr("ECall", ::(loop(e), ::(to_expr_array(el, p), [])));
			case ENew(t, el): expr("ENew", ::(to_tpath(t, p), ::(to_expr_array(el, p), [])));
			case EUnop(op, flag, e): var op = mk_enum("Unop", switch (op) {
			case Increment: "OpIncrement";
			case Decrement: "OpDecrement";
			case Not: "OpNot";
			case Neg: "OpNeg";
			case NegBits: "OpNegBits";
			}, [], p);
				expr("EUnop", ::(op, ::(to_bool( = (flag, Postfix), p), ::(loop(e), []))));
			case EVars(vl): expr("EVars", ::(to_array(function (v, t, e): function p: var fields = ::((new Tuple("name", to_string(v,
												 p))), ::((new Tuple("type", to_opt(to_ctype, t, p))), ::((new Tuple("expr", to_opt(to_expr, e, p))), [])));
												 to_obj(fields, p), vl, p), []));
			case EFunction(name, f): var name = switch (name) {
				case None: to_null(p);
				case Some(name): if (ExtString.String.starts_with(name, "inline_$")) {
						var real_name = String.sub(name, 7, -(String.length(name), 7));
						var e_name = to_string(real_name, p);
						var e_inline = to_string("inline_", p);
						var e_add = (new Tuple(EBinop(OpAdd, e_inline, e_name), p));
						e_add;
					} else {
						to_string(name, p);
					};
				};
				expr("EFunction", ::(name, ::(to_fun(f, p), [])));
			case EBlock(el): expr("EBlock", ::(to_expr_array(el, p), []));
			case EFor(e1, e2): expr("EFor", ::(loop(e1), ::(loop(e2), [])));
			case EIn(e1, e2): expr("EIn", ::(loop(e1), ::(loop(e2), [])));
			case EIf(e1, e2, eelse): expr("EIf", ::(loop(e1), ::(loop(e2), ::(to_opt(to_expr, eelse, p), []))));
			case EWhile(e1, e2, flag): expr("EWhile", ::(loop(e1), ::(loop(e2), ::(to_bool( = (flag, NormalWhile), p), []))));
			case ESwitch(e1, cases, def): 	function scase(Tuple(el, eg, e), p) return {
					to_obj(::((new Tuple("values", to_expr_array(el, p))), ::((new Tuple("guard", to_opt(to_expr, eg, p))), ::((new Tuple("expr", to_opt(to_expr, e, p))), []))), p);
				};
				expr("ESwitch", ::(loop(e1), ::(to_array(scase, cases, p), ::(to_opt(to_opt(to_expr), def, p), []))));
			case ETry(e1, catches): 	function scatch(Tuple(n, t, e), p) return {
					to_obj(::((new Tuple("name", to_string(n, p))), ::((new Tuple("type", to_ctype(t, p))), ::((new Tuple("expr", loop(e))), []))), p);
				};
				expr("ETry", ::(loop(e1), ::(to_array(scatch, catches, p), [])));
			case EReturn(eo): expr("EReturn", ::(to_opt(to_expr, eo, p), []));
			case EBreak: expr("EBreak", []);
			case EContinue: expr("EContinue", []);
			case EUntyped(e): expr("EUntyped", ::(loop(e), []));
			case EThrow(e): expr("EThrow", ::(loop(e), []));
			case ECast(e, ct): expr("ECast", ::(loop(e), ::(to_opt(to_ctype, ct, p), [])));
			case EDisplay(e, flag): expr("EDisplay", ::(loop(e), ::(to_bool(flag, p), [])));
			case EDisplayNew(t): expr("EDisplayNew", ::(to_tpath(t, p), []));
			case ETernary(e1, e2, e3): expr("ETernary", ::(loop(e1), ::(loop(e2), ::(loop(e3), []))));
			case ECheckType(e1, ct): expr("ECheckType", ::(loop(e1), ::(to_ctype(ct, p), [])));
			case EMeta((m, ml, p), e1): switch ((new Tuple(m, ml))) {
				case (Meta.Dollar( | e), _): e1;
				case (Meta.Dollar(a), _): expr("EArrayDecl", switch (fst(e1)) {
				case EArrayDecl(el): ::(to_expr_array(el, p), []);
					case _: ::(e1, []);
					});
				case (Meta.Dollar(b), _): expr("EBlock", ::(e1, []));
				case (Meta.Dollar(v), _): switch (fst(e1)) {
					case EParenthesis(ECheckType(e2, CTPath({ tname = String; tpackage = [] })), _): expr("EConst", ::(mk_enum("Constant",
								"CString", ::(e2, []), pos(e2)), []));
					case EParenthesis(ECheckType(e2, CTPath({ tname = Int; tpackage = [] })), _): expr("EConst", ::(mk_enum("Constant", "CInt",
								::(e2, []), pos(e2)), []));
					case EParenthesis(ECheckType(e2, CTPath({ tname = Float; tpackage = [] })), _): expr("EConst", ::(mk_enum("Constant",
								"CFloat", ::(e2, []), pos(e2)), []));
					case _: (new Tuple(ECall((new Tuple(EField((new Tuple(EField((new Tuple(EField((new Tuple(EConst(Ident("haxe")), p)),
															"macro"), p)), "Context"), p)), "makeExpr"), p)), ::(e, ::(to_pos(pos(e)), []))), p));
					};
				case (Meta.Dollar(i), _): expr("EConst", ::(mk_enum("Constant", "CIdent", ::(e1, []), pos(e1)), []));
				case (Meta.Dollar(p), _): (new Tuple(ECall((new Tuple(EField((new Tuple(EField((new Tuple(EField((new Tuple(EConst(
							Ident("haxe")), p)), "macro"), p)), "MacroStringTools"), p)), "toFieldExpr"), p)), ::(e, [])), p));
				case (Meta.Custom(: pos), ::(pexpr, [])): var old = cur_pos.val;
					cur_pos.val = Some(pexpr);
					var e = loop(e1);
					cur_pos.val = old;
					e;
				case _: expr("EMeta", ::(to_obj(::((new Tuple("name", to_string(fst(Common.MetaInfo.to_string(m)), p))),
													   ::((new Tuple("params", to_expr_array(ml, p))), ::((new Tuple("pos", to_pos(p))), []))), p), ::(loop(e1), [])));
				};
			};
		};
		function to_tparam_decl(p, t) return {
			to_obj(::((new Tuple("name", to_string(t.tp_name, p))), ::((new Tuple("params", (new Tuple(EArrayDecl(List.map(to_tparam_decl(p), t.tp_params)), p)))), ::((new Tuple("constraints", (new Tuple(EArrayDecl(List.map(function t: to_ctype(t, p), t.tp_constraints)), p)))), []))), p);
		};
		function to_type_def(Tuple(t, p)) return {
			switch (t) {
			case EClass(d): var ext = ref(None);
				var impl = ref([]);
				var interf = ref(False);
			List.iter(function case HExtern | HPrivate: [];
				case HInterface: interf.val = True;
					case HExtends(t): ext.val = Some(to_tpath(t, p));
						case HImplements(i): impl.val = ::(to_tpath(i, p), impl.val), d.d_flags);
				to_obj(::((new Tuple("pack", (new Tuple(EArrayDecl([]), p)))), ::((new Tuple("name", to_string(d.d_name, p))),
				::((new Tuple("pos", to_pos(p))), ::((new Tuple("meta", to_meta(d.d_meta, p))), ::((new Tuple("params",
				(new Tuple(EArrayDecl(List.map(to_tparam_decl(p), d.d_params)), p)))), ::((new Tuple("isExtern", to_bool(List.mem(HExtern,
				d.d_flags), p))), ::((new Tuple("kind", mk_enum("TypeDefKind", "TDClass", ::(switch (ext.val) {
			case None: (new Tuple(EConst(Ident("null")), p));
				case Some(t): t;
				}, ::((new Tuple(EArrayDecl(List.rev(impl.val)), p)), ::(to_bool(interf.val, p), []))), p))), ::((new Tuple("fields",
				(new Tuple(EArrayDecl(List.map(function f: to_cfield(f, p), d.d_data)), p)))), [])))))))), p);
			case _: assert False;
			};
		};
		(new Tuple(function e: to_expr(e, snd(e)), to_ctype, to_type_def));
	};

	public static function popt(f, __strmStream.t(_)) return {
		try {
			Some(f(__strm));
		} catch (e: Stream.Failure) {
			None;
		};
	};

	public static function plist(f, __strmStream.t(_)) return {
		switch (try {
			Some(f(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some(v): var l = try {
				plist(f, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			::(v, l);
		case _: [];
		};
	};

	public static function psep(sep, f, __strmStream.t(_)) return {
		switch (try {
			Some(f(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some(v): var s = __strm;
			function loop(__strmStream.t(_)) return {
				switch (Stream.peek(__strm)) {
				case case Some((sep2, _)) if (=(sep2, sep)): Stream.junk(__strm);
					var v = try {
						f(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					var l = try {
						loop(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					::(v, l);
				case ;
				case _: [];
				};
			};
			::(v, loop(s));
		case _: [];
		};
	};

	public static function ident(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(i)), p)): Stream.junk(__strm);
			(new Tuple(i, p));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function dollar_ident(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(i)), p)): Stream.junk(__strm);
			(new Tuple(i, p));
		case Some((Dollar(i), p)): Stream.junk(__strm);
			(new Tuple( ^ ("$", i), p));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function dollar_ident_macro(pack, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(i)), p)): Stream.junk(__strm);
			(new Tuple(i, p));
		case Some((Dollar(i), p)): Stream.junk(__strm);
			(new Tuple( ^ ("$", i), p));
		case Some((Kwd(Macro), p)) if (<>(pack, [])): Stream.junk(__strm);
			(new Tuple("macro", p));
		case Some((Kwd(Extern), p)) if (<>(pack, [])): Stream.junk(__strm);
			(new Tuple("extern", p));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function lower_ident_or_macro(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(i)), p)) if (is_lower_ident(i)): Stream.junk(__strm);
			i;
		case Some((Kwd(Macro), _)): Stream.junk(__strm);
			"macro";
		case Some((Kwd(Extern), _)): Stream.junk(__strm);
			"extern";
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function property_ident(__strmStream.t(_)) return {
		switch (try {
			Some(ident(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some((i, _)): i;
		case _: switch (Stream.peek(__strm)) {
			case case Some((Kwd(Dynamic), _)): Stream.junk(__strm);
				"dynamic";
			case Some((Kwd(Default), _)): Stream.junk(__strm);
				"default";
			case Some((Kwd(Null), _)): Stream.junk(__strm);
				"null";
			case ;
			case _: raise(Stream.Failure);
			};
		};
	};

	public static function get_doc(s) return {
		switch (Stream.peek(s)) {
		case None: None;
		case Some(tk, p): switch (last_doc.val) {
			case None: None;
			case Some(d, pos): last_doc.val = None;
				if ( = (pos, p.pmin)) {
					Some(d);
				} else {
					None;
				};
			};
		};
	};

	public static function comma(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Comma, _)): Stream.junk(__strm);
			[];
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function semicolon(s) return {
		if ( = (fst(last_token(s)), BrClose)) {
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Semicolon, p)): Stream.junk(__strm);
				p;
			case ;
			case _: snd(last_token(s));
			};
		} else {
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Semicolon, p)): Stream.junk(__strm);
				p;
			case ;
			case _: var s = __strm;
				var pos = snd(last_token(s));
				if (do_resume([])) {
					pos;
				} else {
					error(Missing_semicolon, pos);
				};
			};
		};
	};

	public static function parse_file(s) return {
		last_doc.val = None;
		var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Package), _)): Stream.junk(__strm);
			var pack = try {
				parse_package(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Const(Ident(_)), p)) if (=(pack, [])): Stream.junk(__strm);
				error(Custom("Package name must start with a lowercase character"), p);
			case ;
			case _: var _ = semicolon(__strm);
				var l = try {
					parse_type_decls(pack, [], __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((Eof, _)): Stream.junk(__strm);
					(new Tuple(pack, l));
				case _: raise(Stream.Error(""));
				};
			};
		case ;
		case _: var l = parse_type_decls([], [], __strm);
			switch (Stream.peek(__strm)) {
			case Some((Eof, _)): Stream.junk(__strm);
				(new Tuple([], l));
			case _: raise(Stream.Error(""));
			};
		};
	};

	public static function parse_type_decls(pack, acc, s) return {
		try {
			var __strmStream.t(_) = s;
			switch (try {
				Some(parse_type_decl(__strm));
				} catch (e: Stream.Failure) {
					None;
				}) {
			case Some(v): try {
					parse_type_decls(pack, ::(v, acc), __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
			case _: List.rev(acc);
			};
		} catch (e: TypePath([])(Some(name)(False))(b)) {
			List.iter(function d:
			switch (fst(d)) {
		case EImport(t, _): switch (List.rev(t)) {
				case ::((n, _), path) if (&&(=(n, name), List.for_all(function (i, _): is_lower_ident(i),
													 path))): raise(TypePath(List.map(fst, List.rev(path)), Some(name, False), b));
				case _: [];
				};
			case _: [];
			}, acc);
			raise(TypePath(pack, Some(name, True), b));
		};
	};

	public static function parse_type_decl(s) return {
		var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Import), p1)): Stream.junk(__strm);
			parse_import(s, p1);
		case Some((Kwd(Using), p1)): Stream.junk(__strm);
			var t = try {
				parse_type_path(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var p2 = try {
				semicolon(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(EUsing(t), punion(p1, p2)));
		case ;
		case _: var doc = get_doc(__strm);
			var meta = try {
				parse_meta(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var c = try {
				parse_common_flags(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (try {
				Some(parse_enum_flags(__strm));
				} catch (e: Stream.Failure) {
					None;
				}) {
			case Some((n, p1)): var name = try {
					type_name(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				var tl = try {
					parse_constraint_params(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((BrOpen, _)): Stream.junk(__strm);
					var l = try {
						plist(parse_enum, __strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((BrClose, p2)): Stream.junk(__strm);
						(new Tuple(EEnum({ () with d_name = name;
										   d_doc = doc;
										   d_meta = meta;
										   d_params = tl;
										   d_flags = @(List.map(snd, c), n);
										   d_data = l
										 }), punion(p1, p2)));
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case _: switch (try {
					Some(parse_class_flags(__strm));
					} catch (e: Stream.Failure) {
						None;
					}) {
				case Some((n, p1)): var name = try {
						type_name(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					var tl = try {
						parse_constraint_params(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					var hl = try {
						plist(parse_class_herit, __strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((BrOpen, _)): Stream.junk(__strm);
						var Tuple(fl, p2) = try {
							parse_class_fields(False, p1, __strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						(new Tuple(EClass({ () with d_name = name;
											d_doc = doc;
											d_meta = meta;
											d_params = tl;
											d_flags = @(List.map(fst, c), @(n, hl));
											d_data = fl
										  }), punion(p1, p2)));
					case _: raise(Stream.Error(""));
					};
				case _: switch (Stream.peek(__strm)) {
					case case Some((Kwd(Typedef), p1)): Stream.junk(__strm);
						var name = try {
							type_name(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						var tl = try {
							parse_constraint_params(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						switch (Stream.peek(__strm)) {
						case Some((Binop(OpAssign), p2)): Stream.junk(__strm);
							var t = try {
								parse_complex_type(__strm);
							} catch (e: Stream.Failure) {
								raise(Stream.Error(""));
							};
							var s = __strm;
							var __strmStream.t(_) = s;
							switch (Stream.peek(__strm)) {
							case case Some((Semicolon, _)): Stream.junk(__strm);
								[];
							case ;
							case _: [];
							};
							(new Tuple(ETypedef({ () with d_name = name;
												  d_doc = doc;
												  d_meta = meta;
												  d_params = tl;
												  d_flags = List.map(snd, c);
												  d_data = t
												}), punion(p1, p2)));
						case _: raise(Stream.Error(""));
						};
					case Some((Kwd(Abstract), p1)): Stream.junk(__strm);
						var name = try {
							type_name(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						var tl = try {
							parse_constraint_params(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						var st = try {
							parse_abstract_subtype(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						var sl = try {
							plist(parse_abstract_relations, __strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						switch (Stream.peek(__strm)) {
						case Some((BrOpen, _)): Stream.junk(__strm);
							var Tuple(fl, p2) = try {
								parse_class_fields(False, p1, __strm);
							} catch (e: Stream.Failure) {
								raise(Stream.Error(""));
							};
							var flags = List.map(function (_, c):
							switch (c) {
						case EPrivate: APrivAbstract;
						case EExtern: error(Custom("extern abstract not allowed"), p1);
							}, c);
							var flags = switch (st) {
							case None: flags;
							case Some(t): ::(AIsType(t), flags);
							};
							(new Tuple(EAbstract({ () with d_name = name;
												   d_doc = doc;
												   d_meta = meta;
												   d_params = tl;
												   d_flags = @(flags, sl);
												   d_data = fl
												 }), punion(p1, p2)));
						case _: raise(Stream.Error(""));
						};
					case ;
					case _: raise(Stream.Failure);
					};
				};
			};
		};
	};

	public static function parse_class(doc, meta, cflags, need_name, s) return {
		var opt_name = if (need_name) {
			type_name;
		} else {
			function s:
			switch (popt(type_name, s)) {
			case None: "";
			case Some(n): n;
			};
		};
		var __strmStream.t(_) = s;
		var Tuple(n, p1) = parse_class_flags(__strm);
		var name = try {
			opt_name(__strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		var tl = try {
			parse_constraint_params(__strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		var hl = try {
			psep(Comma, parse_class_herit, __strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		switch (Stream.peek(__strm)) {
		case Some((BrOpen, _)): Stream.junk(__strm);
			var Tuple(fl, p2) = try {
				parse_class_fields(!(need_name), p1, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(EClass({ () with d_name = name;
								d_doc = doc;
								d_meta = meta;
								d_params = tl;
								d_flags = @(List.map(fst, cflags), @(n, hl));
								d_data = fl
							  }), punion(p1, p2)));
		case _: raise(Stream.Error(""));
		};
	};

	public static function parse_import(s, p1) return {
		function loop(acc) return {
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Dot, p)): Stream.junk(__strm);
				function resume([]) return {
					type_path(List.map(fst, acc), True);
				};
				if (is_resuming(p)) {
					resume([]);
				} else {
					[];
				};
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Const(Ident(k)), p)): Stream.junk(__strm);
					loop(::((new Tuple(k, p)), acc));
				case Some((Kwd(Macro), p)): Stream.junk(__strm);
					loop(::((new Tuple("macro", p)), acc));
				case Some((Kwd(Extern), p)): Stream.junk(__strm);
					loop(::((new Tuple("extern", p)), acc));
				case Some((Binop(OpMult), _)): Stream.junk(__strm);
					switch (Stream.peek(__strm)) {
					case Some((Semicolon, p2)): Stream.junk(__strm);
						(new Tuple(p2, List.rev(acc), IAll));
					case _: raise(Stream.Error(""));
					};
				case Some((Binop(OpOr), _)) if (do_resume([])): Stream.junk(__strm);
					{
						set_resume(p);
						resume([]);
					};
				case ;
				case _: serror([]);
				};
			case Some((Semicolon, p2)): Stream.junk(__strm);
				(new Tuple(p2, List.rev(acc), INormal));
			case Some((Kwd(In), _)): Stream.junk(__strm);
				switch (Stream.peek(__strm)) {
				case Some((Const(Ident(name)), _)): Stream.junk(__strm);
					switch (Stream.peek(__strm)) {
					case Some((Semicolon, p2)): Stream.junk(__strm);
						(new Tuple(p2, List.rev(acc), IAsName(name)));
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case Some((Const(Ident(as)), _)): Stream.junk(__strm);
				switch (Stream.peek(__strm)) {
				case Some((Const(Ident(name)), _)): Stream.junk(__strm);
					switch (Stream.peek(__strm)) {
					case Some((Semicolon, p2)): Stream.junk(__strm);
						(new Tuple(p2, List.rev(acc), IAsName(name)));
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case ;
			case _: serror([]);
			};
		};
		var Tuple(p2, path, mode) = var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(name)), p)): Stream.junk(__strm);
			loop(::((new Tuple(name, p)), []));
		case ;
		case _: serror([]);
		};
		(new Tuple(EImport(path, mode), punion(p1, p2)));
	};

	public static function parse_abstract_relations(s) return {
		var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(to)), _)): Stream.junk(__strm);
			var t = try {
				parse_complex_type(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			AToType(t);
		case Some((Const(Ident(from)), _)): Stream.junk(__strm);
			var t = try {
				parse_complex_type(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			AFromType(t);
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_abstract_subtype(s) return {
		var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((POpen, _)): Stream.junk(__strm);
			var t = try {
				parse_complex_type(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((PClose, _)): Stream.junk(__strm);
				Some(t);
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: None;
		};
	};

	public static function parse_package(s) return {
		psep(Dot, lower_ident_or_macro, s);
	};

	public static function parse_class_fields(tdecl, p1, s) return {
		var l = parse_class_field_resume(tdecl, s);
		var p2 = var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((BrClose, p2)): Stream.junk(__strm);
			p2;
		case ;
		case _: if (do_resume([])) {
				p1;
			} else {
				serror([]);
			};
		};
		(new Tuple(l, p2));
	};

	public static function parse_class_field_resume(tdecl, s) return {
		if (!(do_resume([]))) {
			plist(parse_class_field, s);
		} else {
			try {
				var c = parse_class_field(s);
				::(c, parse_class_field_resume(tdecl, s));
			} catch (e: Stream.Error(_) | Stream.Failure) {
				function junk(k) return {
					if ( <= (k, 0)) {
						[];
					} else {
						Stream.junk(s);
						junk(-(k, 1));
					};
				};
				function junk_tokens(k) return {
					if ( = (k, 0)) {
						[];
					} else {
						switch (List.rev_map(fst, Stream.npeek(k, s))) {
						case ::(Kwd(Private), _): junk_tokens(-(k, 1));
						case ::(Const(Ident(_)) | Kwd(_), ::(DblDot, ::(At, l))) | ::(Const(Ident(_)) | Kwd(_), ::(At, l)): junk_tokens(
								List.length(l));
						case ::(PClose, l): 	function loop(n) return {
							case []: [];
							case ::(POpen, l): if ( = (n, 0)) {
									l;
								} else {
									loop(-(n, 1), l);
								};
							case ::(PClose, l): loop(+(n, 1), l);
							case ::(_, l): loop(n, l);
							};
							switch (loop(0, l)) {
							case ::(Const(Ident(_)) | Kwd(_), ::(At, l)) | ::(Const(Ident(_)) | Kwd(_), ::(DblDot, ::(At, l))): junk_tokens(
									List.length(l));
							case _: junk(k);
							};
						case _: junk(k);
						};
					};
				};
				function loop(k) return {
					switch (List.rev_map(fst, Stream.npeek(k, s))) {
					case ::(Kwd(_), ::(At, _)) | ::(Kwd(_), ::(DblDot, ::(At, _))): loop(+(k, 1));
					case ::(Const(_), ::(Kwd(Function), _)) | ::(Kwd(New), ::(Kwd(Function), _)): junk_tokens(-(k, 2));
						parse_class_field_resume(tdecl, s);
					case ::(Kwd(Macro), _) | ::(Kwd(Public), _) | ::(Kwd(Static), _) | ::(Kwd(Var), _) | ::(Kwd(Override), _) | ::(Kwd(Dynamic), _) | ::(Kwd(Inline), _)
							: junk_tokens(-(k, 1));
						parse_class_field_resume(tdecl, s);
					case ::(BrClose, _) if (tdecl): junk_tokens(-(k, 1));
						[];
					case ::(Eof, _) | ::(Kwd(Import), _) | ::(Kwd(Using), _) | ::(Kwd(Extern), _) | ::(Kwd(Class), _) | ::(Kwd(Interface), _) | ::(Kwd(Enum), _) | ::(Kwd(Typedef), _) | ::(Kwd(Abstract), _)
							: junk_tokens(-(k, 1));
						[];
					case []: [];
					case _: loop(+(k, 1));
					};
				};
				loop(1);
			};
		};
	};

	public static function parse_common_flags(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Private), _)): Stream.junk(__strm);
			var l = try {
				parse_common_flags(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			::((new Tuple(HPrivate, EPrivate)), l);
		case Some((Kwd(Extern), _)): Stream.junk(__strm);
			var l = try {
				parse_common_flags(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			::((new Tuple(HExtern, EExtern)), l);
		case ;
		case _: [];
		};
	};

	public static function parse_meta_argument_expr(s) return {
		try {
			expr(s);
		} catch (e: Display(e)) {
			switch (fst(e)) {
			case EDisplay(e, _): try {
					type_path(string_list_of_expr_path_raise(e), False);
				} catch (e: Exit) {
					e;
				};
			case _: e;
			};
		};
	};

	public static function parse_meta_params(pname, s) return {
		var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((POpen, p)) if (=(p.pmin, pname.pmax)): Stream.junk(__strm);
			var params = try {
				psep(Comma, parse_meta_argument_expr, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((PClose, _)): Stream.junk(__strm);
				params;
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: [];
		};
	};

	public static function parse_meta_entry(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((At, _)): Stream.junk(__strm);
			var Tuple(name, p) = try {
				meta_name(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var params = try {
				parse_meta_params(p, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(name, params, p));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_meta(__strmStream.t(_)) return {
		switch (try {
			Some(parse_meta_entry(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some(entry): ::(entry, parse_meta(__strm));
		case _: [];
		};
	};

	public static function meta_name(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(i)), p)): Stream.junk(__strm);
			(new Tuple(Meta.Custom(i), p));
		case Some((Kwd(k), p)): Stream.junk(__strm);
			(new Tuple(Meta.Custom(s_keyword(k)), p));
		case Some((DblDot, _)): Stream.junk(__strm);
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Const(Ident(i)), p)): Stream.junk(__strm);
				(new Tuple(Common.MetaInfo.parse(i), p));
			case Some((Kwd(k), p)): Stream.junk(__strm);
				(new Tuple(Common.MetaInfo.parse(s_keyword(k)), p));
			case ;
			case _: raise(Stream.Failure);
			};
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_enum_flags(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Enum), p)): Stream.junk(__strm);
			(new Tuple([], p));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_class_flags(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Class), p)): Stream.junk(__strm);
			(new Tuple([], p));
		case Some((Kwd(Interface), p)): Stream.junk(__strm);
			(new Tuple(::(HInterface, []), p));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_type_hint(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((DblDot, _)): Stream.junk(__strm);
			try {
				parse_complex_type(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_type_opt(__strmStream.t(_)) return {
		try {
			Some(parse_type_hint(__strm));
		} catch (e: Stream.Failure) {
			None;
		};
	};

	public static function parse_complex_type(s) return {
		var t = parse_complex_type_inner(s);
		parse_complex_type_next(t, s);
	};

	public static function parse_structural_extension(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Binop(OpGt), _)): Stream.junk(__strm);
			var t = try {
				parse_type_path(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((Comma, _)): Stream.junk(__strm);
				t;
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_complex_type_inner(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((POpen, _)): Stream.junk(__strm);
			var t = try {
				parse_complex_type(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((PClose, _)): Stream.junk(__strm);
				CTParent(t);
			case _: raise(Stream.Error(""));
			};
		case Some((BrOpen, p1)): Stream.junk(__strm);
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (try {
				Some(parse_type_anonymous(False, __strm));
				} catch (e: Stream.Failure) {
					None;
				}) {
			case Some(l): CTAnonymous(l);
			case _: switch (try {
					Some(parse_structural_extension(__strm));
					} catch (e: Stream.Failure) {
						None;
					}) {
				case Some(t): var s = __strm;
					var tl = ::(t, plist(parse_structural_extension, s));
					var __strmStream.t(_) = s;
					switch (try {
						Some(parse_type_anonymous(False, __strm));
						} catch (e: Stream.Failure) {
							None;
						}) {
					case Some(l): CTExtend(tl, l);
					case _: var Tuple(l, _) = parse_class_fields(True, p1, __strm);
						CTExtend(tl, l);
					};
				case _: switch (try {
						Some(parse_class_fields(True, p1, __strm));
						} catch (e: Stream.Failure) {
							None;
						}) {
					case Some((l, _)): CTAnonymous(l);
					case _: serror([]);
					};
				};
			};
		case Some((Question, _)): Stream.junk(__strm);
			var t = try {
				parse_complex_type_inner(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			CTOptional(t);
		case ;
		case _: var t = parse_type_path(__strm);
			CTPath(t);
		};
	};

	public static function parse_type_path(s) return {
		parse_type_path1([], s);
	};

	public static function parse_type_path1(pack, __strmStream.t(_)) return {
		switch (try {
			Some(dollar_ident_macro(pack, __strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some((name, p)): var s = __strm;
			if (is_lower_ident(name)) {
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Dot, p)): Stream.junk(__strm);
					if (is_resuming(p)) {
						raise(TypePath(List.rev(::(name, pack)), None, False));
					} else {
						parse_type_path1(::(name, pack), s);
					};
				case Some((Semicolon, _)): Stream.junk(__strm);
					error(Custom("Type name should start with an uppercase letter"), p);
				case ;
				case _: serror([]);
				};
			} else {
				var sub = var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Dot, p)): Stream.junk(__strm);
					var s = __strm;
					if (is_resuming(p)) {
						raise(TypePath(List.rev(pack), Some(name, False), False));
					} else {
						var __strmStream.t(_) = s;
						switch (Stream.peek(__strm)) {
						case case Some((Const(Ident(name)), _)) if (!(is_lower_ident(name))): Stream.junk(__strm);
							Some(name);
						case Some((Binop(OpOr), _)) if (do_resume([])): Stream.junk(__strm);
							{
								set_resume(p);
								raise(TypePath(List.rev(pack), Some(name, False), False));
							};
						case ;
						case _: serror([]);
						};
					};
				case ;
				case _: None;
				};
				var params = var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Binop(OpLt), _)): Stream.junk(__strm);
					var l = try {
						psep(Comma, parse_type_path_or_const, __strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((Binop(OpGt), _)): Stream.junk(__strm);
						l;
					case _: raise(Stream.Error(""));
					};
				case ;
				case _: [];
				};
				{
					() with tpackage = List.rev(pack);
					tname = name;
					tparams = params;
					tsub = sub
				};
			};
		case _: switch (Stream.peek(__strm)) {
			case case Some((Binop(OpOr), _)) if (do_resume([])): Stream.junk(__strm);
				raise(TypePath(List.rev(pack), None, False));
			case ;
			case _: raise(Stream.Failure);
			};
		};
	};

	public static function type_name(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(name)), p)): Stream.junk(__strm);
			if (is_lower_ident(name)) {
				error(Custom("Type name should start with an uppercase letter"), p);
			} else {
				name;
			};
		case Some((Dollar(name), _)): Stream.junk(__strm);
			^ ("$", name);
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_type_path_or_const(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((BkOpen, p1)): Stream.junk(__strm);
			var l = try {
				parse_array_decl(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((BkClose, p2)): Stream.junk(__strm);
				TPExpr(EArrayDecl(l), punion(p1, p2));
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: switch (try {
				Some(parse_complex_type(__strm));
				} catch (e: Stream.Failure) {
					None;
				}) {
			case Some(t): TPType(t);
			case _: switch (Stream.peek(__strm)) {
				case case Some((Const(c), p)): Stream.junk(__strm);
					TPExpr(EConst(c), p);
				case ;
				case _: switch (try {
						Some(expr(__strm));
						} catch (e: Stream.Failure) {
							None;
						}) {
					case Some(e): TPExpr(e);
					case _: serror([]);
					};
				};
			};
		};
	};

	public static function parse_complex_type_next(t, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Arrow, _)): Stream.junk(__strm);
			var t2 = try {
				parse_complex_type(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (t2) {
			case CTFunction(args, r): CTFunction(::(t, args), r);
			case _: CTFunction(::(t, []), t2);
			};
		case ;
		case _: t;
		};
	};

	public static function parse_type_anonymous(opt, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Question, _)) if (!(opt)): Stream.junk(__strm);
			parse_type_anonymous(True, __strm);
		case ;
		case _: var Tuple(name, p1) = ident(__strm);
			var t = try {
				parse_type_hint(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var s = __strm;
			function next(p2, acc) return {
				::({
					() with cff_name = name;
					cff_meta = if (opt) {
						::((new Tuple(Meta.Optional, [], p1)), []);
					} else {
						[];
					};
					cff_access = [];
					cff_doc = None;
					cff_kind = FVar(Some(t), None);
					cff_pos = punion(p1, p2)
				}, acc);
			};
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((BrClose, p2)): Stream.junk(__strm);
				next(p2, []);
			case Some((Comma, p2)): Stream.junk(__strm);
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((BrClose, _)): Stream.junk(__strm);
					next(p2, []);
				case ;
				case _: switch (try {
						Some(parse_type_anonymous(False, __strm));
						} catch (e: Stream.Failure) {
							None;
						}) {
					case Some(l): next(p2, l);
					case _: serror([]);
					};
				};
			case ;
			case _: serror([]);
			};
		};
	};

	public static function parse_enum(s) return {
		var doc = get_doc(s);
		var meta = parse_meta(s);
		var __strmStream.t(_) = s;
		var Tuple(name, p1) = ident(__strm);
		var params = try {
			parse_constraint_params(__strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		var s = __strm;
		var args = var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((POpen, _)): Stream.junk(__strm);
			var l = try {
				psep(Comma, parse_enum_param, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((PClose, _)): Stream.junk(__strm);
				l;
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: [];
		};
		var t = parse_type_opt(s);
		var p2 = var __strmStream.t(_) = s;
		try {
			semicolon(__strm);
		} catch (e: Stream.Failure) {
			serror([]);
		};
		{
			() with ec_name = name;
			ec_doc = doc;
			ec_meta = meta;
			ec_args = args;
			ec_params = params;
			ec_type = t;
			ec_pos = punion(p1, p2)
		};
	};

	public static function parse_enum_param(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Question, _)): Stream.junk(__strm);
			var Tuple(name, _) = try {
				ident(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var t = try {
				parse_type_hint(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(name, True, t));
		case ;
		case _: var Tuple(name, _) = ident(__strm);
			var t = try {
				parse_type_hint(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(name, False, t));
		};
	};

	public static function parse_class_field(s) return {
		var doc = get_doc(s);
		var __strmStream.t(_) = s;
		var meta = parse_meta(__strm);
		var al = try {
			parse_cf_rights(True, [], __strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		var s = __strm;
		var Tuple(name, pos, k) = var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Var), p1)): Stream.junk(__strm);
			var Tuple(name, _) = try {
				dollar_ident(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((POpen, _)): Stream.junk(__strm);
				var i1 = try {
					property_ident(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((Comma, _)): Stream.junk(__strm);
					var i2 = try {
						property_ident(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((PClose, _)): Stream.junk(__strm);
						var t = parse_type_opt(s);
						var Tuple(e, p2) = var __strmStream.t(_) = s;
						switch (Stream.peek(__strm)) {
						case case Some((Binop(OpAssign), _)): Stream.junk(__strm);
							var e = try {
								toplevel_expr(__strm);
							} catch (e: Stream.Failure) {
								raise(Stream.Error(""));
							};
							var p2 = try {
								semicolon(__strm);
							} catch (e: Stream.Failure) {
								raise(Stream.Error(""));
							};
							(new Tuple(Some(e), p2));
						case Some((Semicolon, p2)): Stream.junk(__strm);
							(new Tuple(None, p2));
						case ;
						case _: serror([]);
						};
						(new Tuple(name, punion(p1, p2), FProp(i1, i2, t, e)));
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case ;
			case _: var t = parse_type_opt(__strm);
				var s = __strm;
				var Tuple(e, p2) = var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Binop(OpAssign), _)): Stream.junk(__strm);
					var e = try {
						toplevel_expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					var p2 = try {
						semicolon(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					(new Tuple(Some(e), p2));
				case Some((Semicolon, p2)): Stream.junk(__strm);
					(new Tuple(None, p2));
				case ;
				case _: serror([]);
				};
				(new Tuple(name, punion(p1, p2), FVar(t, e)));
			};
		case Some((Kwd(Function), p1)): Stream.junk(__strm);
			var name = try {
				parse_fun_name(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var pl = try {
				parse_constraint_params(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((POpen, _)): Stream.junk(__strm);
				var al = try {
					psep(Comma, parse_fun_param, __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((PClose, _)): Stream.junk(__strm);
					var t = try {
						parse_type_opt(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					var s = __strm;
					var Tuple(e, p2) = var __strmStream.t(_) = s;
					switch (try {
						Some(toplevel_expr(__strm));
						} catch (e: Stream.Failure) {
							None;
						}) {
					case Some(e): var s = __strm;
						try {
							ignore(semicolon(s));
						} catch (e: Error(Missing_semicolon)(p)) {
							display_error.val(Missing_semicolon, p);
						};
						(new Tuple(Some(e), pos(e)));
					case _: switch (Stream.peek(__strm)) {
						case case Some((Semicolon, p)): Stream.junk(__strm);
							(new Tuple(None, p));
						case ;
						case _: serror([]);
						};
					};
					var f = { () with f_params = pl;
							  f_args = al;
							  f_type = t;
							  f_expr = e
							};
					(new Tuple(name, punion(p1, p2), FFun(f)));
				case _: raise(Stream.Error(""));
				};
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: if ( = (al, [])) {
				raise(Stream.Failure);
			} else {
				serror([]);
			};
		};
		{
			() with cff_name = name;
			cff_doc = doc;
			cff_meta = meta;
			cff_access = al;
			cff_pos = pos;
			cff_kind = k
		};
	};

	public static function parse_cf_rights(allow_static, l, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Static), _)) if (allow_static): Stream.junk(__strm);
			try {
				parse_cf_rights(False, ::(AStatic, l), __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case Some((Kwd(Macro), _)) if (!(List.mem(AMacro, l))): Stream.junk(__strm);
			try {
				parse_cf_rights(allow_static, ::(AMacro, l), __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case Some((Kwd(Public), _)) if (!(||(List.mem(APublic, l), List.mem(APrivate, l)))): Stream.junk(__strm);
			try {
				parse_cf_rights(allow_static, ::(APublic, l), __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case Some((Kwd(Private), _)) if (!(||(List.mem(APublic, l), List.mem(APrivate, l)))): Stream.junk(__strm);
			try {
				parse_cf_rights(allow_static, ::(APrivate, l), __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case Some((Kwd(Override), _)) if (!(List.mem(AOverride, l))): Stream.junk(__strm);
			try {
				parse_cf_rights(False, ::(AOverride, l), __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case Some((Kwd(Dynamic), _)) if (!(List.mem(ADynamic, l))): Stream.junk(__strm);
			try {
				parse_cf_rights(allow_static, ::(ADynamic, l), __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case Some((Kwd(Inline), _)): Stream.junk(__strm);
			try {
				parse_cf_rights(allow_static, ::(AInline, l), __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case ;
		case _: l;
		};
	};

	public static function parse_fun_name(__strmStream.t(_)) return {
		switch (try {
			Some(dollar_ident(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some((name, _)): name;
		case _: switch (Stream.peek(__strm)) {
			case case Some((Kwd(New), _)): Stream.junk(__strm);
				"new";
			case ;
			case _: raise(Stream.Failure);
			};
		};
	};

	public static function parse_fun_param(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Question, _)): Stream.junk(__strm);
			var Tuple(name, _) = try {
				dollar_ident(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var t = try {
				parse_type_opt(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var c = try {
				parse_fun_param_value(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(name, True, t, c));
		case ;
		case _: var Tuple(name, _) = dollar_ident(__strm);
			var t = try {
				parse_type_opt(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var c = try {
				parse_fun_param_value(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(name, False, t, c));
		};
	};

	public static function parse_fun_param_value(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Binop(OpAssign), _)): Stream.junk(__strm);
			try {
				Some(toplevel_expr(__strm));
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
		case ;
		case _: None;
		};
	};

	public static function parse_fun_param_type(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Question, _)): Stream.junk(__strm);
			var name = try {
				ident(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var t = try {
				parse_type_hint(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(name, True, t));
		case ;
		case _: var name = ident(__strm);
			var t = try {
				parse_type_hint(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(name, False, t));
		};
	};

	public static function parse_constraint_params(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Binop(OpLt), _)): Stream.junk(__strm);
			var l = try {
				psep(Comma, parse_constraint_param, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((Binop(OpGt), _)): Stream.junk(__strm);
				l;
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: [];
		};
	};

	public static function parse_constraint_param(__strmStream.t(_)) return {
		var meta = parse_meta(__strm);
		var name = try {
			type_name(__strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		var s = __strm;
		var params = var __strmStream.t(_) = s;
		[];
		var ctl = var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((DblDot, _)): Stream.junk(__strm);
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((POpen, _)): Stream.junk(__strm);
				var l = try {
					psep(Comma, parse_complex_type, __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((PClose, _)): Stream.junk(__strm);
					l;
				case _: raise(Stream.Error(""));
				};
			case ;
			case _: switch (try {
					Some(parse_complex_type(__strm));
					} catch (e: Stream.Failure) {
						None;
					}) {
				case Some(t): ::(t, []);
				case _: serror([]);
				};
			};
		case ;
		case _: [];
		};
		{
			() with tp_name = name;
			tp_params = params;
			tp_constraints = ctl;
			tp_meta = meta
		};
	};

	public static function parse_class_herit(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Extends), _)): Stream.junk(__strm);
			var t = try {
				parse_type_path(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			HExtends(t);
		case Some((Kwd(Implements), _)): Stream.junk(__strm);
			var t = try {
				parse_type_path(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			HImplements(t);
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function block1(__strmStream.t(_)) return {
		switch (try {
			Some(dollar_ident(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some((name, p)): block2(name, Ident(name), p, __strm);
		case _: switch (Stream.peek(__strm)) {
			case case Some((Const(String(name)), p)): Stream.junk(__strm);
				block2(quote_ident(name), String(name), p, __strm);
			case ;
			case _: var b = block([], __strm);
				EBlock(b);
			};
		};
	};

	public static function block2(name, ident, p, s) return {
		var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((DblDot, _)): Stream.junk(__strm);
			var e = try {
				expr(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var l = try {
				parse_obj_decl(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			EObjectDecl(::((new Tuple(name, e)), l));
		case ;
		case _: var e = expr_next((new Tuple(EConst(ident), p)), s);
			try {
				var _ = semicolon(s);
				var b = block(::(e, []), s);
				EBlock(b);
			} catch (e: Error(err)(p)) {
				display_error.val(err, p);
				EBlock(block(::(e, []), s));
			};
		};
	};

	public static function block(acc, s) return {
		try {
			var e = try {
				parse_block_elt(s);
			} catch (e: Display(e)) {
				display((new Tuple(EBlock(List.rev(::(e, acc))), snd(e))));
			};
			block(::(e, acc), s);
		} catch (e: T) {
			McOr(McArr(PaId(IdAcc(IdUid(<...>), IdUid(<...>))), ExNil, ExApp(ExId(IdAcc(<...>, <...>)), ExId(IdLid(<...>)))),
				 McOr(McArr(PaApp(PaId(<...>), PaAny), ExNil, ExLet(ReNil, BiEq(<...>, <...>), ExSeq(<...>))), McArr(PaApp(PaApp(<...>,
					 <...>), PaId(<...>)), ExNil, ExSeq(ExSem(<...>, <...>)))))			case Stream.Failure: List.rev(acc);
		case Stream.Error(_): var Tuple(tk, pos) = switch (Stream.peek(s)) {
			case None: last_token(s);
			case Some(t): t;
			};
			display_error.val(Unexpected(tk), pos);
			block(acc, s);
		case Error(e, p): display_error.val(e, p);
			block(acc, s);
		};
	};

	public static function parse_block_elt(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Var), p1)): Stream.junk(__strm);
			var vl = try {
				parse_var_decls(p1, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var p2 = try {
				semicolon(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(EVars(vl), punion(p1, p2)));
		case Some((Kwd(Inline), p1)): Stream.junk(__strm);
			switch (Stream.peek(__strm)) {
			case Some((Kwd(Function), _)): Stream.junk(__strm);
				var e = try {
					parse_function(p1, True, __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				var _ = try {
					semicolon(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				e;
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: var e = expr(__strm);
			var _ = try {
				semicolon(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			e;
		};
	};

	public static function parse_obj_decl(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Comma, _)): Stream.junk(__strm);
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (try {
				Some(ident(__strm));
				} catch (e: Stream.Failure) {
					None;
				}) {
			case Some((name, _)): switch (Stream.peek(__strm)) {
				case Some((DblDot, _)): Stream.junk(__strm);
					var e = try {
						expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					var l = try {
						parse_obj_decl(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					::((new Tuple(name, e)), l);
				case _: raise(Stream.Error(""));
				};
			case _: switch (Stream.peek(__strm)) {
				case case Some((Const(String(name)), _)): Stream.junk(__strm);
					switch (Stream.peek(__strm)) {
					case Some((DblDot, _)): Stream.junk(__strm);
						var e = try {
							expr(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						var l = try {
							parse_obj_decl(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						::((new Tuple(quote_ident(name), e)), l);
					case _: raise(Stream.Error(""));
					};
				case ;
				case _: [];
				};
			};
		case ;
		case _: [];
		};
	};

	public static function parse_array_decl(__strmStream.t(_)) return {
		switch (try {
			Some(expr(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some(e): var s = __strm;
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Comma, _)): Stream.junk(__strm);
				var l = try {
					parse_array_decl(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				::(e, l);
			case ;
			case _: ::(e, []);
			};
		case _: [];
		};
	};

	public static function parse_var_decl_head(__strmStream.t(_)) return {
		var Tuple(name, _) = dollar_ident(__strm);
		var t = try {
			parse_type_opt(__strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		(new Tuple(name, t));
	};

	public static function parse_var_assignment(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Binop(OpAssign), p1)): Stream.junk(__strm);
			var s = __strm;
			var __strmStream.t(_) = s;
			try {
				Some(expr(__strm));
			} catch (e: Stream.Failure) {
				error(Custom("expression expected after ="), p1);
			};
		case ;
		case _: None;
		};
	};

	public static function parse_var_decls_next(vl, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Comma, p1)): Stream.junk(__strm);
			var Tuple(name, t) = try {
				parse_var_decl_head(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var s = __strm;
			try {
				var eo = parse_var_assignment(s);
				parse_var_decls_next(::((new Tuple(name, t, eo)), vl), s);
			} catch (e: Display(e)) {
				var v = (new Tuple(name, t, Some(e)));
				var e = (new Tuple(EVars(List.rev(::(v, vl))), punion(p1, pos(e))));
				display(e);
			};
		case ;
		case _: vl;
		};
	};

	public static function parse_var_decls(p1, __strmStream.t(_)) return {
		switch (try {
			Some(parse_var_decl_head(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some((name, t)): var s = __strm;
			var eo = parse_var_assignment(s);
			List.rev(parse_var_decls_next(::((new Tuple(name, t, eo)), []), s));
		case _: error(Custom("Missing variable identifier"), p1);
		};
	};

	public static function parse_var_decl(__strmStream.t(_)) return {
		var Tuple(name, t) = parse_var_decl_head(__strm);
		var eo = try {
			parse_var_assignment(__strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		(new Tuple(name, t, eo));
	};

	public static function inline_function(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Inline), _)): Stream.junk(__strm);
			switch (Stream.peek(__strm)) {
			case Some((Kwd(Function), p1)): Stream.junk(__strm);
				(new Tuple(True, p1));
			case _: raise(Stream.Error(""));
			};
		case Some((Kwd(Function), p1)): Stream.junk(__strm);
			(new Tuple(False, p1));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function reify_expr(e) return {
		var Tuple(to_expr, _, _) = reify(in_macro.val);
		var e = to_expr(e);
		(new Tuple(ECheckType(e, CTPath({
			() with tpackage = ::("haxe", ::("macro", []));
			tname = "Expr";
			tsub = None;
			tparams = []
		})), pos(e)));
	};

	public static function parse_macro_expr(p, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((DblDot, _)): Stream.junk(__strm);
			var t = try {
				parse_complex_type(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var Tuple(_, to_type, _) = reify(in_macro.val);
			var t = to_type(t, p);
			(new Tuple(ECheckType(t, CTPath({ () with tpackage = ::("haxe", ::("macro", []));
											  tname = "Expr";
											  tsub = Some("ComplexType");
											  tparams = []
											})), p));
		case Some((Kwd(Var), p1)): Stream.junk(__strm);
			var vl = try {
				psep(Comma, parse_var_decl, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			reify_expr((new Tuple(EVars(vl), p1)));
		case ;
		case _: switch (try {
				Some(parse_class(None, [], [], False, __strm));
				} catch (e: Stream.Failure) {
					None;
				}) {
			case Some(d): var Tuple(_, _, to_type) = reify(in_macro.val);
				(new Tuple(ECheckType(to_type(d), CTPath({ () with tpackage = ::("haxe", ::("macro", []));
									  tname = "Expr";
									  tsub = Some("TypeDefinition");
									  tparams = []
														 })), p));
			case _: var e = secure_expr(__strm);
				reify_expr(e);
			};
		};
	};

	public static function parse_function(p1, inl, __strmStream.t(_)) return {
		var name = popt(dollar_ident, __strm);
		var pl = try {
			parse_constraint_params(__strm);
		} catch (e: Stream.Failure) {
			raise(Stream.Error(""));
		};
		switch (Stream.peek(__strm)) {
		case Some((POpen, _)): Stream.junk(__strm);
			var al = try {
				psep(Comma, parse_fun_param, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((PClose, _)): Stream.junk(__strm);
				var t = try {
					parse_type_opt(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				var s = __strm;
				function make(e) return {
					var f = {
						() with f_params = pl;
						f_type = t;
						f_args = al;
						f_expr = Some(e)
					};
					(new Tuple(EFunction(switch (name) {
				case None: None;
				case Some(name, _): Some(if (inl) {
						^ ("inline_", name);
						} else {
							name;
						});
					}, f), punion(p1, pos(e))));
				};
				try {
					expr_next(make(secure_expr(s)), s);
				} catch (e: Display(e)) {
					display(make(e));
				};
			case _: raise(Stream.Error(""));
			};
		case _: raise(Stream.Error(""));
		};
	};

	public static function expr(__strmStream.t(_)) return {
		switch (try {
			Some(parse_meta_entry(__strm));
			} catch (e: Stream.Failure) {
				None;
			}) {
		case Some((name, params, p)): var s = __strm;
			try {
				make_meta(name, params, secure_expr(s), p);
			} catch (e: Display(e)) {
				display(make_meta(name, params, e, p));
			};
		case _: switch (Stream.peek(__strm)) {
			case case Some((BrOpen, p1)): Stream.junk(__strm);
				var s = __strm;
				if (is_resuming(p1)) {
					display((new Tuple(EDisplay((new Tuple(EObjectDecl([]), p1)), False), p1)));
				} else {
					[];
				};
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Binop(OpOr), p2)) if (do_resume([])): Stream.junk(__strm);
					{
						set_resume(p1);
						display((new Tuple(EDisplay((new Tuple(EObjectDecl([]), p1)), False), p1)));
					};
				case ;
				case _: var b = block1(__strm);
					switch (Stream.peek(__strm)) {
					case Some((BrClose, p2)): Stream.junk(__strm);
						var s = __strm;
						var e = (new Tuple(b, punion(p1, p2)));
						switch (b) {
						case EObjectDecl(_): expr_next(e, s);
						case _: e;
						};
					case _: raise(Stream.Error(""));
					};
				};
			case Some((Kwd(Macro), p)): Stream.junk(__strm);
				parse_macro_expr(p, __strm);
			case Some((Kwd(Var), p1)): Stream.junk(__strm);
				var v = try {
					parse_var_decl(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				(new Tuple(EVars(::(v, [])), p1));
			case Some((Const(c), p)): Stream.junk(__strm);
				expr_next((new Tuple(EConst(c), p)), __strm);
			case Some((Kwd(This), p)): Stream.junk(__strm);
				expr_next((new Tuple(EConst(Ident("this")), p)), __strm);
			case Some((Kwd( True), p)): Stream.junk(__strm);
				expr_next((new Tuple(EConst(Ident("true")), p)), __strm);
			case Some((Kwd( False), p)): Stream.junk(__strm);
				expr_next((new Tuple(EConst(Ident("false")), p)), __strm);
			case Some((Kwd(Null), p)): Stream.junk(__strm);
				expr_next((new Tuple(EConst(Ident("null")), p)), __strm);
			case Some((Kwd(Cast), p1)): Stream.junk(__strm);
				var s = __strm;
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((POpen, pp)): Stream.junk(__strm);
					var e = try {
						expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					var s = __strm;
					var __strmStream.t(_) = s;
					switch (Stream.peek(__strm)) {
					case case Some((Comma, _)): Stream.junk(__strm);
						var t = try {
							parse_complex_type(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						switch (Stream.peek(__strm)) {
						case Some((PClose, p2)): Stream.junk(__strm);
							expr_next((new Tuple(ECast(e, Some(t)), punion(p1, p2))), __strm);
						case _: raise(Stream.Error(""));
						};
					case ;
					case _: switch (try {
							Some(parse_type_hint(__strm));
							} catch (e: Stream.Failure) {
								None;
							}) {
						case Some(t): switch (Stream.peek(__strm)) {
							case Some((PClose, p2)): Stream.junk(__strm);
								var ep = (new Tuple(EParenthesis(ECheckType(e, t), punion(p1, p2)), punion(p1, p2)));
								expr_next((new Tuple(ECast(ep, None), punion(p1, pos(ep)))), __strm);
							case _: raise(Stream.Error(""));
							};
						case _: switch (Stream.peek(__strm)) {
							case case Some((Const(Ident(is)), _)): Stream.junk(__strm);
								var t = try {
									parse_type_path(__strm);
								} catch (e: Stream.Failure) {
									raise(Stream.Error(""));
								};
								switch (Stream.peek(__strm)) {
								case Some((PClose, p2)): Stream.junk(__strm);
									var e_is = make_is(e, t, punion(p1, p2));
									expr_next((new Tuple(ECast(e_is, None), punion(p1, pos(e_is)))), s);
								case _: raise(Stream.Error(""));
								};
							case Some((PClose, p2)): Stream.junk(__strm);
								var ep = expr_next((new Tuple(EParenthesis(e), punion(pp, p2))), __strm);
								expr_next((new Tuple(ECast(ep, None), punion(p1, pos(ep)))), __strm);
							case ;
							case _: serror([]);
							};
						};
					};
				case ;
				case _: var e = secure_expr(__strm);
					expr_next((new Tuple(ECast(e, None), punion(p1, pos(e)))), s);
				};
			case Some((Kwd(Throw), p)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				(new Tuple(EThrow(e), p));
			case Some((Kwd(New), p1)): Stream.junk(__strm);
				var t = try {
					parse_type_path(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((POpen, p)): Stream.junk(__strm);
					var s = __strm;
					if (is_resuming(p)) {
						display((new Tuple(EDisplayNew(t), punion(p1, p))));
					} else {
						[];
					};
					var __strmStream.t(_) = s;
					switch (try {
						Some(psep(Comma, expr, __strm));
						} catch (e: Stream.Failure) {
							None;
						}) {
					case Some(al): switch (Stream.peek(__strm)) {
						case Some((PClose, p2)): Stream.junk(__strm);
							expr_next((new Tuple(ENew(t, al), punion(p1, p2))), __strm);
						case _: raise(Stream.Error(""));
						};
					case _: serror([]);
					};
				case _: raise(Stream.Error(""));
				};
			case Some((POpen, p1)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				var s = __strm;
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((PClose, p2)): Stream.junk(__strm);
					expr_next((new Tuple(EParenthesis(e), punion(p1, p2))), __strm);
				case ;
				case _: switch (try {
						Some(parse_type_hint(__strm));
						} catch (e: Stream.Failure) {
							None;
						}) {
					case Some(t): switch (Stream.peek(__strm)) {
						case Some((PClose, p2)): Stream.junk(__strm);
							expr_next((new Tuple(EParenthesis(ECheckType(e, t), punion(p1, p2)), punion(p1, p2))), __strm);
						case _: raise(Stream.Error(""));
						};
					case _: switch (Stream.peek(__strm)) {
						case case Some((Const(Ident(is)), _)): Stream.junk(__strm);
							var t = try {
								parse_type_path(__strm);
							} catch (e: Stream.Failure) {
								raise(Stream.Error(""));
							};
							switch (Stream.peek(__strm)) {
							case Some((PClose, p2)): Stream.junk(__strm);
								expr_next(make_is(e, t, punion(p1, p2)), s);
							case _: raise(Stream.Error(""));
							};
						case ;
						case _: serror([]);
						};
					};
				};
			case Some((BkOpen, p1)): Stream.junk(__strm);
				var l = try {
					parse_array_decl(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((BkClose, p2)): Stream.junk(__strm);
					expr_next((new Tuple(EArrayDecl(l), punion(p1, p2))), __strm);
				case _: raise(Stream.Error(""));
				};
			case Some((Kwd(Function), p1)): Stream.junk(__strm);
				try {
					parse_function(p1, False, __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
			case Some((Unop(op), p1)) if (is_prefix(op)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				make_unop(op, e, p1);
			case Some((Binop(OpSub), p1)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				function neg(s) return {
					if ( = (s0, '-')) {
						String.sub(s, 1, -(String.length(s), 1));
					} else {
						^ ("-", s);
					};
				};
				switch (make_unop(Neg, e, p1)) {
				case (EUnop(Neg, Prefix, (EConst(Int(i)), pc)), p): (new Tuple(EConst(Int(neg(i))), p));
				case (EUnop(Neg, Prefix, (EConst(Float(j)), pc)), p): (new Tuple(EConst(Float(neg(j))), p));
				case e: e;
				};
			case Some((Kwd(For), p)): Stream.junk(__strm);
				switch (Stream.peek(__strm)) {
				case Some((POpen, _)): Stream.junk(__strm);
					var it = try {
						expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((PClose, _)): Stream.junk(__strm);
						var s = __strm;
						try {
							var e = secure_expr(s);
							(new Tuple(EFor(it, e), punion(p, pos(e))));
						} catch (e: Display(e)) {
							display((new Tuple(EFor(it, e), punion(p, pos(e)))));
						};
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case Some((Kwd(If), p)): Stream.junk(__strm);
				switch (Stream.peek(__strm)) {
				case Some((POpen, _)): Stream.junk(__strm);
					var cond = try {
						expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((PClose, _)): Stream.junk(__strm);
						var e1 = try {
							expr(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						var s = __strm;
						var e2 = var __strmStream.t(_) = s;
						switch (Stream.peek(__strm)) {
						case case Some((Kwd(Else), _)): Stream.junk(__strm);
							try {
								Some(expr(__strm));
							} catch (e: Stream.Failure) {
								raise(Stream.Error(""));
							};
						case ;
						case _: switch (Stream.npeek(2, s)) {
							case ::((Semicolon, _), ::((Kwd(Else), _), [])): Stream.junk(s);
								Stream.junk(s);
								Some(secure_expr(s));
							case _: None;
							};
						};
						(new Tuple(EIf(cond, e1, e2), punion(p, switch (e2) {
					case None: pos(e1);
						case Some(e): pos(e);
						})));
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case Some((Kwd(Return), p)): Stream.junk(__strm);
				var e = try {
					popt(expr, __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				(new Tuple(EReturn(e), switch (e) {
			case None: p;
			case Some(e): punion(p, pos(e));
				}));
			case Some((Kwd(Break), p)): Stream.junk(__strm);
				(new Tuple(EBreak, p));
			case Some((Kwd(Continue), p)): Stream.junk(__strm);
				(new Tuple(EContinue, p));
			case Some((Kwd(While), p1)): Stream.junk(__strm);
				switch (Stream.peek(__strm)) {
				case Some((POpen, _)): Stream.junk(__strm);
					var cond = try {
						expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((PClose, _)): Stream.junk(__strm);
						var s = __strm;
						try {
							var e = secure_expr(s);
							(new Tuple(EWhile(cond, e, NormalWhile), punion(p1, pos(e))));
						} catch (e: Display(e)) {
							display((new Tuple(EWhile(cond, e, NormalWhile), punion(p1, pos(e)))));
						};
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case Some((Kwd(Do), p1)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((Kwd(While), _)): Stream.junk(__strm);
					switch (Stream.peek(__strm)) {
					case Some((POpen, _)): Stream.junk(__strm);
						var cond = try {
							expr(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						switch (Stream.peek(__strm)) {
						case Some((PClose, _)): Stream.junk(__strm);
							(new Tuple(EWhile(cond, e, DoWhile), punion(p1, pos(e))));
						case _: raise(Stream.Error(""));
						};
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case Some((Kwd(Switch), p1)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((BrOpen, _)): Stream.junk(__strm);
					var Tuple(cases, def) = try {
						parse_switch_cases(e, [], __strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					switch (Stream.peek(__strm)) {
					case Some((BrClose, p2)): Stream.junk(__strm);
						(new Tuple(ESwitch(e, cases, def), punion(p1, p2)));
					case _: raise(Stream.Error(""));
					};
				case _: raise(Stream.Error(""));
				};
			case Some((Kwd(Try), p1)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				var cl = try {
					plist(parse_catch(e), __strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				(new Tuple(ETry(e, cl), p1));
			case Some((IntInterval(i), p1)): Stream.junk(__strm);
				var e2 = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				make_binop(OpInterval, (new Tuple(EConst(Int(i)), p1)), e2);
			case Some((Kwd(Untyped), p1)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				(new Tuple(EUntyped(e), punion(p1, pos(e))));
			case Some((Dollar(v), p)): Stream.junk(__strm);
				expr_next((new Tuple(EConst(Ident( ^ ("$", v))), p)), __strm);
			case ;
			case _: raise(Stream.Failure);
			};
		};
	};

	public static function expr_next(e1, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((BrOpen, p1)) if (is_dollar_ident(e1)): Stream.junk(__strm);
			var eparam = try {
				expr(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((BrClose, p2)): Stream.junk(__strm);
				var s = __strm;
				switch (fst(e1)) {
				case EConst(Ident(n)): expr_next((new Tuple(EMeta((new Tuple(Common.MetaInfo.from_string(n), [], snd(e1))), eparam),
													  punion(p1, p2))), s);
				case _: assert False;
				};
			case _: raise(Stream.Error(""));
			};
		case Some((Dot, p)): Stream.junk(__strm);
			var s = __strm;
			if (is_resuming(p)) {
				display((new Tuple(EDisplay(e1, False), p)));
			} else {
				[];
			};
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Kwd(Macro), p2)) if (=(p.pmax, p2.pmin)): Stream.junk(__strm);
				expr_next((new Tuple(EField(e1, "macro"), punion(pos(e1), p2))), __strm);
			case Some((Kwd(New), p2)) if (=(p.pmax, p2.pmin)): Stream.junk(__strm);
				expr_next((new Tuple(EField(e1, "new"), punion(pos(e1), p2))), __strm);
			case Some((Const(Ident(f)), p2)) if (=(p.pmax, p2.pmin)): Stream.junk(__strm);
				expr_next((new Tuple(EField(e1, f), punion(pos(e1), p2))), __strm);
			case Some((Dollar(v), p2)): Stream.junk(__strm);
				expr_next((new Tuple(EField(e1, ^ ("$", v)), punion(pos(e1), p2))), __strm);
			case Some((Binop(OpOr), p2)) if (do_resume([])): Stream.junk(__strm);
				{
					set_resume(p);
					display((new Tuple(EDisplay(e1, False), p)));
				};
			case ;
			case _: switch (e1) {
				case (EConst(Int(v)), p2) if (=(p2.pmax, p.pmin)): expr_next((new Tuple(EConst(Float( ^ (v, "."))), punion(p, p2))), s);
				case _: serror([]);
				};
			};
		case Some((POpen, p1)): Stream.junk(__strm);
			var s = __strm;
			if (is_resuming(p1)) {
				display((new Tuple(EDisplay(e1, True), p1)));
			} else {
				[];
			};
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Binop(OpOr), p2)) if (do_resume([])): Stream.junk(__strm);
				{
					set_resume(p1);
					display((new Tuple(EDisplay(e1, True), p1)));
				};
			case ;
			case _: switch (try {
					Some(parse_call_params(e1, __strm));
					} catch (e: Stream.Failure) {
						None;
					}) {
				case Some(params): switch (Stream.peek(__strm)) {
					case Some((PClose, p2)): Stream.junk(__strm);
						expr_next((new Tuple(ECall(e1, params), punion(pos(e1), p2))), __strm);
					case _: raise(Stream.Error(""));
					};
				case _: serror([]);
				};
			};
		case Some((BkOpen, _)): Stream.junk(__strm);
			var e2 = try {
				expr(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((BkClose, p2)): Stream.junk(__strm);
				expr_next((new Tuple(EArray(e1, e2), punion(pos(e1), p2))), __strm);
			case _: raise(Stream.Error(""));
			};
		case Some((Binop(OpGt), p1)): Stream.junk(__strm);
			var s = __strm;
			var __strmStream.t(_) = s;
			switch (Stream.peek(__strm)) {
			case case Some((Binop(OpGt), p2)) if (=(p1.pmax, p2.pmin)): Stream.junk(__strm);
				var s = __strm;
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Binop(OpGt), p3)) if (=(p2.pmax, p3.pmin)): Stream.junk(__strm);
					var __strmStream.t(_) = s;
					switch (Stream.peek(__strm)) {
					case case Some((Binop(OpAssign), p4)) if (=(p3.pmax, p4.pmin)): Stream.junk(__strm);
						var e2 = try {
							expr(__strm);
						} catch (e: Stream.Failure) {
							raise(Stream.Error(""));
						};
						make_binop(OpAssignOp(OpUShr), e1, e2);
					case ;
					case _: var e2 = secure_expr(__strm);
						make_binop(OpUShr, e1, e2);
					};
				case Some((Binop(OpAssign), p3)) if (=(p2.pmax, p3.pmin)): Stream.junk(__strm);
					var e2 = try {
						expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					make_binop(OpAssignOp(OpShr), e1, e2);
				case ;
				case _: var e2 = secure_expr(__strm);
					make_binop(OpShr, e1, e2);
				};
			case Some((Binop(OpAssign), p2)) if (=(p1.pmax, p2.pmin)): Stream.junk(__strm);
				make_binop(OpGte, e1, secure_expr(__strm));
			case ;
			case _: var e2 = secure_expr(__strm);
				make_binop(OpGt, e1, e2);
			};
		case Some((Binop(op), _)): Stream.junk(__strm);
			var s = __strm;
			try {
				var __strmStream.t(_) = s;
				switch (try {
					Some(expr(__strm));
					} catch (e: Stream.Failure) {
						None;
					}) {
				case Some(e2): make_binop(op, e1, e2);
				case _: serror([]);
				};
			} catch (e: Display(e2)) {
				raise(Display(make_binop(op, e1, e2)));
			};
		case Some((Unop(op), p)) if (is_postfix(e1, op)): Stream.junk(__strm);
			expr_next((new Tuple(EUnop(op, Postfix, e1), punion(pos(e1), p))), __strm);
		case Some((Question, _)): Stream.junk(__strm);
			var e2 = try {
				expr(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((DblDot, _)): Stream.junk(__strm);
				var e3 = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				(new Tuple(ETernary(e1, e2, e3), punion(pos(e1), pos(e3))));
			case _: raise(Stream.Error(""));
			};
		case Some((Kwd(In), _)): Stream.junk(__strm);
			var e2 = try {
				expr(__strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(EIn(e1, e2), punion(pos(e1), pos(e2))));
		case ;
		case _: e1;
		};
	};

	public static function parse_guard(__strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(If), p1)): Stream.junk(__strm);
			switch (Stream.peek(__strm)) {
			case Some((POpen, _)): Stream.junk(__strm);
				var e = try {
					expr(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				switch (Stream.peek(__strm)) {
				case Some((PClose, _)): Stream.junk(__strm);
					e;
				case _: raise(Stream.Error(""));
				};
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_switch_cases(eswitch, cases, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Default), p1)): Stream.junk(__strm);
			switch (Stream.peek(__strm)) {
			case Some((DblDot, _)): Stream.junk(__strm);
				var s = __strm;
				var b = try {
					block([], s);
				} catch (e: Display(e)) {
					display((new Tuple(ESwitch(eswitch, cases, Some(Some(e))), punion(pos(eswitch), pos(e)))));
				};
				var b = switch (b) {
				case []: None;
				case _: Some(EBlock(b), p1);
				};
				var Tuple(l, def) = parse_switch_cases(eswitch, cases, s);
				switch (def) {
				case None: [];
				case Some(_): error(Duplicate_default, p1);
				};
				(new Tuple(l, Some(b)));
			case _: raise(Stream.Error(""));
			};
		case Some((Kwd(Case), p1)): Stream.junk(__strm);
			var el = try {
				psep(Comma, expr, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			var eg = try {
				popt(parse_guard, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((DblDot, _)): Stream.junk(__strm);
				var s = __strm;
				switch (el) {
				case []: error(Custom("case without a pattern is not allowed"), p1);
				case _: var b = try {
						block([], s);
					} catch (e: Display(e)) {
						display((new Tuple(ESwitch(eswitch, List.rev(::((new Tuple(el, eg, Some(e))), cases)), None), punion(pos(eswitch),
										   pos(e)))));
					};
					var b = switch (b) {
					case []: None;
					case _: Some(EBlock(b), p1);
					};
					parse_switch_cases(eswitch, ::((new Tuple(el, eg, b)), cases), s);
				};
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: (new Tuple(List.rev(cases), None));
		};
	};

	public static function parse_catch(etry, __strmStream.t(_)) return {
		switch (Stream.peek(__strm)) {
		case case Some((Kwd(Catch), p)): Stream.junk(__strm);
			switch (Stream.peek(__strm)) {
			case Some((POpen, _)): Stream.junk(__strm);
				var Tuple(name, _) = try {
					dollar_ident(__strm);
				} catch (e: Stream.Failure) {
					raise(Stream.Error(""));
				};
				var s = __strm;
				var __strmStream.t(_) = s;
				switch (try {
					Some(parse_type_hint(__strm));
					} catch (e: Stream.Failure) {
						None;
					}) {
				case Some(t): switch (Stream.peek(__strm)) {
					case Some((PClose, _)): Stream.junk(__strm);
						var s = __strm;
						try {
							(new Tuple(name, t, secure_expr(s)));
						} catch (e: Display(e)) {
							display((new Tuple(ETry(etry, ::((new Tuple(name, t, e)), [])), punion(pos(etry), pos(e)))));
						};
					case _: raise(Stream.Error(""));
					};
				case _: switch (Stream.peek(__strm)) {
					case case Some((_, p)): Stream.junk(__strm);
						error(Missing_type, p);
					case ;
					case _: raise(Stream.Failure);
					};
				};
			case _: raise(Stream.Error(""));
			};
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_call_params(ec, s) return {
		var e = try {
			var __strmStream.t(_) = s;
			try {
				Some(expr(__strm));
			} catch (e: Stream.Failure) {
				None;
			};
		} catch (e: Display(e)) {
			display((new Tuple(ECall(ec, ::(e, [])), punion(pos(ec), pos(e)))));
		};
		function loop(acc) return {
			try {
				var __strmStream.t(_) = s;
				switch (Stream.peek(__strm)) {
				case case Some((Comma, _)): Stream.junk(__strm);
					var e = try {
						expr(__strm);
					} catch (e: Stream.Failure) {
						raise(Stream.Error(""));
					};
					loop(::(e, acc));
				case ;
				case _: List.rev(acc);
				};
			} catch (e: Display(e)) {
				display((new Tuple(ECall(ec, List.rev(::(e, acc))), punion(pos(ec), pos(e)))));
			};
		};
		switch (e) {
		case None: [];
		case Some(e): loop(::(e, []));
		};
	};

	public static function parse_macro_cond(allow_op, s) return {
		var __strmStream.t(_) = s;
		switch (Stream.peek(__strm)) {
		case case Some((Const(Ident(t)), p)): Stream.junk(__strm);
			parse_macro_ident(allow_op, t, p, s);
		case Some((Const(String(s)), p)): Stream.junk(__strm);
			(new Tuple(None, (new Tuple(EConst(String(s)), p))));
		case Some((Const(Int(i)), p)): Stream.junk(__strm);
			(new Tuple(None, (new Tuple(EConst(Int(i)), p))));
		case Some((Const(Float(f)), p)): Stream.junk(__strm);
			(new Tuple(None, (new Tuple(EConst(Float(f)), p))));
		case Some((Kwd(k), p)): Stream.junk(__strm);
			parse_macro_ident(allow_op, s_keyword(k), p, s);
		case Some((POpen, p1)): Stream.junk(__strm);
			var Tuple(_, e) = try {
				parse_macro_cond(True, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			switch (Stream.peek(__strm)) {
			case Some((PClose, p2)): Stream.junk(__strm);
				var e = (new Tuple(EParenthesis(e), punion(p1, p2)));
				if (allow_op) {
					parse_macro_op(e, s);
				} else {
					(new Tuple(None, e));
				};
			case _: raise(Stream.Error(""));
			};
		case Some((Unop(op), p)): Stream.junk(__strm);
			var Tuple(tk, e) = try {
				parse_macro_cond(allow_op, __strm);
			} catch (e: Stream.Failure) {
				raise(Stream.Error(""));
			};
			(new Tuple(tk, make_unop(op, e, p)));
		case ;
		case _: raise(Stream.Failure);
		};
	};

	public static function parse_macro_ident(allow_op, t, p, s) return {
		var e = (new Tuple(EConst(Ident(t)), p));
		if (!(allow_op)) {
			(new Tuple(None, e));
		} else {
			parse_macro_op(e, s);
		};
	};

	public static function parse_macro_op(e, s) return {
		switch (Stream.peek(s)) {
		case Some(Binop(op), _): Stream.junk(s);
			var op = switch (Stream.peek(s)) {
			case Some(Binop(OpAssign), _) if (=(op, OpGt)): Stream.junk(s);
				OpGte;
			case _: op;
			};
			var Tuple(tk, e2) = try {
				parse_macro_cond(True, s);
			} catch (e: Stream.Failure) {
				serror([]);
			};
			(new Tuple(tk, make_binop(op, e, e2)));
		case tk: (new Tuple(tk, e));
		};
	};

	public static function toplevel_expr(s) return {
		try {
			expr(s);
		} catch (e: Display(e)) {
			e;
		};
	};

	public static function secure_expr(s) return {
		var __strmStream.t(_) = s;
		try {
			expr(__strm);
		} catch (e: Stream.Failure) {
			serror([]);
		};
	};

	public static function is_true(match) return switch (match) {
	case TBool(False) | TNull | TFloat(undefined) | TString(): False;
	case _: True;
	};

	public static function cmp(v1, v2) return {
		switch ((new Tuple(v1, v2))) {
		case (TNull, TNull): 0;
		case (TFloat(a), TFloat(b)): compare(a, b);
		case (TString(a), TString(b)): compare(a, b);
		case (TBool(a), TBool(b)): compare(a, b);
		case (TString(a), TFloat(b)): compare(float_of_string(a), b);
		case (TFloat(a), TString(b)): compare(a, float_of_string(b));
		case _: raise(Exit);
		};
	};

	public static function eval(ctx, Tuple(e, p)) return {
		switch (e) {
		case EConst(Ident(i)): try {
				TString(Common.raw_defined_value(ctx, i));
			} catch (e: Not_found) {
				TNull;
			};
		case EConst(String(s)): TString(s);
		case EConst(Int(i)): TFloat(float_of_string(i));
		case EConst(Float(f)): TFloat(float_of_string(f));
		case EBinop(OpBoolAnd, e1, e2): TBool( && (is_true(eval(ctx, e1)), is_true(eval(ctx, e2))));
		case EBinop(OpBoolOr, e1, e2): TBool( || (is_true(eval(ctx, e1)), is_true(eval(ctx, e2))));
		case EUnop(Not, _, e): TBool(!(is_true(eval(ctx, e))));
		case EParenthesis(e): eval(ctx, e);
		case EBinop(op, e1, e2): var v1 = eval(ctx, e1);
			var v2 = eval(ctx, e2);
			function compare(op) return {
				TBool(try {
					op(cmp(v1, v2), 0);
				} catch (e: _) {
					False;
				});
			};
			switch (op) {
			case OpEq: compare( = );
			case OpNotEq: compare(<>);
			case OpGt: compare( > );
			case OpGte: compare( >= );
			case OpLt: compare( < );
			case OpLte: compare( <= );
			case _: error(Custom("Unsupported operation"), p);
			};
		case _: error(Custom("Invalid condition expression"), p);
		};
	};

	public static function parse(ctx, code) return {
		var old = Lexer.save([]);
		var old_cache = cache.val;
		var mstack = ref([]);
		cache.val = DynArray.create([]);
		last_doc.val = None;
		in_macro.val = Common.defined(ctx, Common.Define.Macro);
		Lexer.skip_header(code);
		var sraw = Stream.from(function _: Some(Lexer.token(code)));
		function next_token([]) return {
			process_token(Lexer.token(code));
		};
		function process_token(tk) return {
			switch (fst(tk)) {
			case Comment(s): var tk = next_token([]);
				if (use_doc.val) {
					var l = String.length(s);
					if ( && ( > (l, 0), = (s0, '*'))) {
						last_doc.val = Some(String.sub(s, 1, -(l, if ( && ( > (l, 1), = (s - (l, 1), '*'))) {
						2;
					} else {
						1;
					})), snd(tk).pmin);
					} else {
						[];
					};
				} else {
					[];
				};
				tk;
			case CommentLine(s): next_token([]);
			case Sharp(end): switch (mstack.val) {
				case []: tk;
				case ::(_, l): mstack.val = l;
					next_token([]);
				};
			case Sharp(else) | Sharp(elseif): switch (mstack.val) {
				case []: tk;
				case ::(_, l): mstack.val = l;
					process_token(skip_tokens(snd(tk), False));
				};
			case Sharp(if): process_token(enter_macro(snd(tk)));
			case Sharp(error): switch (Lexer.token(code)) {
				case (Const(String(s)), p): error(Custom(s), p);
				case _: error(Unimplemented, snd(tk));
				};
			case Sharp(line): var line = switch (next_token([])) {
				case (Const(Int(s)), _): int_of_string(s);
				case (t, p): error(Unexpected(t), p);
				};
				Lexer.cur.val.Lexer.lline = -(line, 1);
				next_token([]);
			case _: tk;
			};
		};
		function enter_macro(p) return {
			var Tuple(tk, e) = parse_macro_cond(False, sraw);
			var tk = switch (tk) {
			case None: Lexer.token(code);
			case Some(tk): tk;
			};
			if ( || (is_true(eval(ctx, e)), switch (fst(e)) {
			case EConst(Ident(macro)) if (=(Common.unique_full_path(p.pfile), resume_display.val.pfile)): True;
				case _: False;
				})) {
				mstack.val = ::(p, mstack.val);
				tk;
			} else {
				skip_tokens_loop(p, True, tk);
			};
		};
		function skip_tokens_loop(p, test, tk) return {
			switch (fst(tk)) {
			case Sharp(end): Lexer.token(code);
			case Sharp(elseif) | Sharp(else) if (!(test)): skip_tokens(p, test);
			case Sharp(else): mstack.val = ::(snd(tk), mstack.val);
				Lexer.token(code);
			case Sharp(elseif): enter_macro(snd(tk));
			case Sharp(if): skip_tokens_loop(p, test, skip_tokens(p, False));
			case Eof: if (do_resume([])) {
					tk;
				} else {
					error(Unclosed_macro, p);
				};
			case _: skip_tokens(p, test);
			};
		};
		function skip_tokens(p, test) return {
			skip_tokens_loop(p, test, Lexer.token(code));
		};
		var s = Stream.from(function _: var t = next_token([]);
							DynArray.add(cache.val, t);
							Some(t));
		try {
			var l = parse_file(s);
			switch (mstack.val) {
			case ::(p, _) if (!(do_resume([]))): error(Unclosed_macro, p);
			case _: [];
			};
			cache.val = old_cache;
			Lexer.restore(old);
			l;
		} catch (e: T) {
			McOr(McArr(PaOrp(PaApp(PaId(<...>), PaAny), PaId(IdAcc(<...>, <...>))), ExNil, ExLet(ReNil, BiEq(PaId(<...>), ExMat(<...>,
					   <...>)), ExSeq(ExSem(<...>, <...>)))), McArr(PaId(IdLid(e)), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExSem(<...>,
		<...>)))))			case Stream.Error(_) | Stream.Failure: var last = switch (Stream.peek(s)) {
			case None: last_token(s);
			case Some(t): t;
			};
			Lexer.restore(old);
			cache.val = old_cache;
			error(Unexpected(fst(last)), pos(last));
		case e: Lexer.restore(old);
			cache.val = old_cache;
			raise(e);
		};
	}
}
;
