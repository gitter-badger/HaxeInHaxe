import Ast;
import Type;
import Common;
import Typecore;

class Analyzer {
	public static var s_expr = s_expr(s_type(print_context([])));

	public static var s_expr_pretty = s_expr_pretty("", s_type(print_context([])));

	public static function debug(e) return {
		print_endline(s_expr(e));
	};

	public static function debug_pretty(s, e) return {
		Printf.printf("%s %s\n", s, s_expr_pretty(e));
	};

	public static var flag_no_check = "no_check";

	public static var flag_check = "check";

	public static var flag_no_const_propagation = "no_const_propagation";

	public static var flag_const_propagation = "const_propagation";

	public static var flag_no_local_dce = "no_local_dce";

	public static var flag_local_dce = "local_dce";

	public static var flag_ignore = "ignore";

	public static var flag_no_simplification = "no_simplification";

	public static var flag_check_has_effect = "check_has_effect";

	public static var flag_no_check_has_effect = "no_check_has_effect";

	public static function has_analyzer_option(meta, s) return {
		try {
			function loop(ml) return {
				switch (ml) {
				case ::((Meta.Analyzer, el, _), ml): if (List.exists(function (e, p):
					switch (e) {
					case EConst(Ident(s2)) if (=(s, s2)): True;
						case _: False;
						}, el)) {
						True;
					} else {
						loop(ml);
					};
				case ::(_, ml): loop(ml);
				case []: False;
				};
			};
			loop(meta);
		} catch (e: Not_found) {
			False;
		};
	};

	public static function is_ignored(meta) return {
		try {
			function loop(ml) return {
				switch (ml) {
				case ::((Meta.Analyzer, el, _), ml): if (List.exists(function (e, p):
					switch (e) {
					case EConst(Ident(s2)) if (=(flag_ignore, s2)): True;
						case _: False;
						}, el)) {
						True;
					} else {
						loop(ml);
					};
				case ::((Meta.HasUntyped, _, _), _): True;
				case ::(_, ml): loop(ml);
				case []: False;
				};
			};
			loop(meta);
		} catch (e: Not_found) {
			False;
		};
	};

	public static function get_type_meta(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: raise(Not_found);
			case Some(t): get_type_meta(t);
			};
		case TLazy(f): get_type_meta(f.val([]));
		case TInst(c, _): c.cl_meta;
		case TEnum(en, _): en.e_meta;
		case TAbstract(a, _): a.a_meta;
		case TType(t, _): t.t_meta;
		case TAnon(_) | TFun(_) | TDynamic(_): raise(Not_found);
		};
	};

	public static function type_has_analyzer_option(t, s) return {
		try {
			has_analyzer_option(get_type_meta(t), s);
		} catch (e: Not_found) {
			False;
		};
	};

	public static function is_enum_type(t) return {
		switch (follow(t)) {
		case TEnum(_): True;
		case _: False;
		};
	};

	public static function awkward_get_enum_index(com, e) return {
		switch (e.eexpr) {
		case TArray(e1, { eexpr = TConst(TInt(i)) }) if (&&(=(com.platform, Js), &&(=(Int32.to_int(i), 1), is_enum_type(e1.etype))))
				: e1;
		case TCall({ eexpr = TField(e1, FDynamic(__Index)) }, []) if (&&(=(com.platform, Cpp), is_enum_type(e1.etype))): e1;
		case TField(e1, FDynamic(index)) if (&&(=(com.platform, Neko), is_enum_type(e1.etype))): e1;
		case TParenthesis(e1) | TCast(e1, None) | TMeta(_, e1): awkward_get_enum_index(com, e1);
		case _: raise(Not_found);
		};
	};

	public static function lrev_iter(f, el) return {
		switch (el) {
		case ::(e, el): lrev_iter(f, el);
			f(e);
		case []: [];
		};
	};

	public static function rev_iter(f, e) return {
		switch (e.eexpr) {
		case TConst(_) | TLocal(_) | TBreak | TContinue | TTypeExpr(_): [];
		case TArray(e1, e2) | TBinop(_, e1, e2) | TFor(_, e1, e2) | TWhile(e1, e2, _): f(e2);
			f(e1);
		case TThrow(e) | TField(e, _) | TEnumParameter(e, _, _) | TParenthesis(e) | TCast(e, _) | TUnop(_, _, e) | TMeta(_, e): f(
				e);
		case TArrayDecl(el) | TNew(_, _, el) | TBlock(el): lrev_iter(f, el);
		case TObjectDecl(fl): lrev_iter(function (_, e): f(e), fl);
		case TCall(e, el): f(e);
			lrev_iter(f, el);
		case TVar(v, eo): switch (eo) {
			case None: [];
			case Some(e): f(e);
			};
		case TFunction(fu): f(fu.tf_expr);
		case TIf(e, e1, e2): switch (e2) {
			case None: [];
			case Some(e): f(e);
			};
			f(e1);
			f(e);
		case TSwitch(e, cases, def): switch (def) {
			case None: [];
			case Some(e): f(e);
			};
			lrev_iter(function (el, e2): lrev_iter(f, el);
					  f(e2), cases);
			f(e);
		case TTry(e, catches): lrev_iter(function (_, e): f(e), catches);
			f(e);
		case TReturn(eo): switch (eo) {
			case None: [];
			case Some(e): f(e);
			};
		};
	}
}
