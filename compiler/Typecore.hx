import Common;
import Type;

enum With_type {
	NoValue;
	Value;
	WithType(value: T);
	WithTypeResume(value: T);
};

typedef Type_patch = {
	tp_type : Option<Ast.Complex_type>,
	tp_remove : Bool,
	tp_meta : Ast.Metadata
};

enum Current_fun {
	FunMember;
	FunStatic;
	FunConstructor;
	FunMemberAbstract;
	FunMemberClassLocal;
	FunMemberAbstractLocal;
};

enum Macro_mode {
	MExpr;
	MBuild;
	MMacroType;
};

enum Typer_pass {
	PBuildModule;
	PBuildClass;
	PTypeField;
	PCheckConstraint;
	PForce;
	PFinal;
};

typedef Typer_globals = {
	types_module : Hashtbl<Path, Path>,
	modules : Hashtbl<Path, Module_def>,
	delayed : List < Tuple < Typer_pass, List < Unit -> Unit >>>,
	debug_delayed : List < Tuple < Typer_pass, List < Tuple < Unit -> Unit, String, Typer >>> >,
	doinline : Bool,
	core_api : Option<Typer>,
	macros : Option < Tuple < Unit -> Unit, Typer >>,
	std : Module_def,
	hook_generate : List < Unit -> Unit >,
	type_patches : Hashtbl<Path, Tuple<Hashtbl<Tuple<String, Bool>, Type_patch>, Type_patch>>,
	global_metadata : List<Tuple<List<String>, Ast.Metadata_entry, Tuple<Bool, Bool, Bool>>>,
	get_build_infos : Unit -> Option<Tuple<Module_type, List<T>, List<Ast.Class_field>>>,
	delayed_macros : DynArray < Unit -> Unit >,
	global_using : List<Tclass>,
	do_inherit : Typer -> Type.Tclass -> Ast.Pos -> Ast.Class_flag -> Bool,
	do_create : Common.Context -> Typer,
	do_macro : Typer -> Macro_mode -> Path -> String -> List<Ast.Expr> -> Ast.Pos -> Option<Ast.Expr>,
	do_load_module : Typer -> Path -> Pos -> Module_def,
	do_optimize : Typer -> Texpr -> Texpr,
	do_build_instance : Typer -> Module_type -> Pos -> Tuple < List<Tuple<String, T>>, Path, List<T> -> T >
};

typedef Typer_module = {
	curmod : Module_def,
	module_types : List<Module_type>,
	module_using : List<Tclass>,
	module_globals : PMap<String, Tuple<Module_type, String>>,
	wildcard_packages : List<String>,
	module_imports : List<Ast.Import>
};

typedef Typer = {
	com : Context,
	t : Basic_types,
	g : Typer_globals,
	meta : Metadata,
	this_stack : List<Texpr>,
	with_type_stack : List<With_type>,
	call_argument_stack : List<Ast.Expr>,
	pass : Typer_pass,
	m : Typer_module,
	curclass : Tclass,
	tthis : T,
	type_params : List<Tuple<String, T>>,
	curfield : Tclass_field,
	untyped : Bool,
	in_super_call : Bool,
	in_loop : Bool,
	in_display : Bool,
	in_macro : Bool,
	macro_depth : Int,
	curfun : Current_fun,
	ret : T,
	locals : PMap<String, Tvar>,
	opened : List<Anon_status>,
	vthis : Option<Tvar>,
	on_error : Typer -> String -> Pos -> Unit
};

enum Call_error {
	Not_enough_arguments(value: List<Tuple<String, Bool, T>>);
	Too_many_arguments;
	Could_not_unify(value: Error_msg);
	Cannot_skip_non_nullable(value: String);
};

enum Error_msg {
	Module_not_found(value: Path);
	Type_not_found(value: PathString);
	Unify(value: List<Unify_error>);
	Custom(value: String);
	Unknown_ident(value: String);
	Stack(value: Error_msgError_msg);
	Call_error(value: Call_error);
};

class /*exception*/ Fatal_error {

};

class /*exception*/ Forbid_package {

};

class /*exception*/ Error {

};

class /*exception*/ DisplayTypes {

};

class /*exception*/ DisplayPosition {

};

class Typecore {
	public static var make_call_ref = ref(function _: function _: function _: function _: function _: assert False) : ref(
										  typer -> texpr -> list(texpr) -> t -> pos -> texpr);

	public static var type_expr_ref = ref(function _: function _: function _: assert False) : ref(
										  typer -> Ast.expr -> with_type -> texpr);

	public static var type_module_type_ref = ref(function _: function _: function _: function _: assert False) : ref(
				typer -> module_type -> option(list(t)) -> pos -> texpr);

	public static var unify_min_ref = ref(function _: function _: assert False) : ref(typer -> list(texpr) -> t);

	public static var match_expr_ref = ref(function _: function _: function _: function _: function _: function _: assert
										   False) : ref(typer -> Ast.expr -> list(Tuple<list(Ast.expr) * option(Ast.expr) * option(Ast.expr)>) -> option(option(
												   Ast.expr)) -> with_type -> Ast.pos -> decision_tree);

	public static var get_pattern_locals_ref = ref(function _: function _: function _: assert False) : ref(
				typer -> Ast.expr -> Type.t -> PMap.t(string)(Tuple<tvar * pos>));

	public static var get_constructor_ref = ref(function _: function _: function _: function _: assert False) : ref(
			typer -> tclass -> list(t) -> Ast.pos -> Tuple<t * tclass_field>);

	public static var cast_or_unify_ref = ref(function _: function _: function _: function _: assert False) : ref(
			typer -> t -> texpr -> Ast.pos -> texpr);

	public static var find_array_access_raise_ref = ref(function _: function _: function _: function _: function _: function _:
			assert False) : ref(typer -> tabstract -> tparams -> texpr -> option(texpr) -> pos ->
								Tuple<tclass_field * t * t * texpr * option(texpr)>);

	public static function levenshtein(a, b) return {
		var x = Array.init(String.length(a), function i: ai);
		var y = Array.init(String.length(b), function i: bi);
		function minimum(xint, y, z) return {
			function m'(aint, b) return {
			if (<(a, b)) {
			a;
		} else {
			b;
		};
		};
			m'(m'(x, y), z);
		};
			function init_matrix(n, m) return {
			var init_col = Array.init(m);
			Array.init(n, function case 0: init_col(function j: j);
			case i: init_col(function case 0: i;
			case _: 0));
		};
			switch ((new Tuple(Array.length(x), Array.length(y)))) {
			case (0, n): n;
			case (m, 0): m;
			case (m, n): var matrix = init_matrix(+(m, 1), +(n, 1));
			for(i in /*to*/1...m){
			var s = matrixi;
			var t = matrix-(i, 1);
			for(j in /*to*/1...n){
			var cost = abs(compare(x-(i, 1), y-(j, 1)));
			sj = minimum(+(tj, 1), +(s-(j, 1), 1), +(t-(j, 1), cost));
		};
		};
			matrixmn;
		};
		};

			public static function string_error_raise(s, sl, msg) return {
			if (=(sl, [])) {
			msg;
		} else {
			var cl = List.map(function s2: (new Tuple(s2, levenshtein(s, s2))), sl);
			var cl = List.sort(function (_, c1): function (_, c2): compare(c1, c2), cl);
			function loop(sl) return {
			switch (sl) {
			case ::((s2, i), sl) if(<=(i, /(min(String.length(s), String.length(s2)), 3))): ::(s2, loop(sl));
			case _: [];
		};
		};
			switch (loop(cl)) {
			case []: raise(Not_found);
			case ::(s, []): Printf.sprintf("%s [Suggestion: %s]", msg, s);
			case sl: Printf.sprintf("%s [Suggestions: %s]", msg, String.concat(", ", sl));
		};
		};
		};

			public static function string_error(s, sl, msg) return {
			try {
			string_error_raise(s, sl, msg);
		} catch(e:Not_found) {
			msg;
		};
		};

			public static function string_source(t) return {
			switch (follow(t)) {
			case TInst(c, _): List.map(function cf: cf.cf_name, c.cl_ordered_fields);
			case TAnon(a): PMap.fold(function cf: function acc: ::(cf.cf_name, acc), a.a_fields, []);
			case TAbstract({ a_impl = Some(c) }, _): List.map(function cf: cf.cf_name, c.cl_ordered_statics);
			case _: [];
		};
		};

			public static function short_type(ctx, t) return {
			var tstr = s_type(ctx, t);
			if (>(String.length(tstr), 150)) {
			^(String.sub(tstr, 0, 147), "...");
		} else {
			tstr;
		};
		};

			public static function unify_error_msg(ctx) return {
			case Cannot_unify(t1, t2): ^(s_type(ctx, t1), ^(" should be ", s_type(ctx, t2)));
			case Invalid_field_type(s): ^("Invalid type for field ", ^(s, " :"));
			case Has_no_field(t, n): string_error(n, string_source(t), ^(short_type(ctx, t), ^(" has no field ", n)));
			case Has_no_runtime_field(t, n): ^(s_type(ctx, t), ^(".", ^(n, " is not accessible at runtime")));
			case Has_extra_field(t, n): ^(short_type(ctx, t), ^(" has extra field ", n));
			case Invalid_kind(f, a, b): switch ((new Tuple(a, b))) {
			case (Var(va), Var(vb)): var Tuple(name, stra, strb) = if (=(va.v_read, vb.v_read)) {
			(new Tuple("setter", s_access(False, va.v_write), s_access(False, vb.v_write)));
		} else {
			if (=(va.v_write, vb.v_write)) {
			(new Tuple("getter", s_access(True, va.v_read), s_access(True, vb.v_read)));
		} else {
			(new Tuple("access", ^("[", ^(s_access(True, va.v_read), ^(",", ^(s_access(False, va.v_write), "]")))), ^("[", ^(s_access(True, vb.v_read), ^(",", ^(s_access(False, vb.v_write), "]"))))));
		};
		};
			^("Inconsistent ", ^(name, ^(" for field ", ^(f, ^(" : ", ^(stra, ^(" should be ", strb)))))));
			case _: ^("Field ", ^(f, ^(" is ", ^(s_kind(a), ^(" but should be ", s_kind(b))))));
		};
			case Invalid_visibility(n): ^("The field ", ^(n, " is not public"));
			case Not_matching_optional(n): ^("Optional attribute of parameter ", ^(n, " differs"));
			case Cant_force_optional: "Optional parameters can't be forced";
			case Invariant_parameter(_): "Type parameters are invariant";
			case Constraint_failure(name): ^("Constraint check failure for ", name);
			case Missing_overload(cf, t): ^(cf.cf_name, ^(" has no overload for ", s_type(ctx, t)));
				case Unify_custom(msg): msg;
			};

				public static function error_msg(match) return switch (match) {
				case Module_not_found(m): ^("Type not found : ", Ast.s_type_path(m));
				case Type_not_found(m, t): ^("Module ", ^(Ast.s_type_path(m), ^(" does not define type ", t)));
				case Unify(l): var ctx = print_context([]);
				String.concat("\n", List.map(unify_error_msg(ctx), l));
				case Unknown_ident(s): ^("Unknown identifier : ", s);
				case Custom(s): s;
				case Stack(m1, m2): ^(error_msg(m1), ^("\n", error_msg(m2)));
				case Call_error(err): s_call_error(err);
			};

				public static function s_call_error(match) return switch (match) {
				case Not_enough_arguments(tl): var pctx = print_context([]);
				^("Not enough arguments, expected ", String.concat(", ", List.map(function (n, _, t): ^(n, ^(" : ", short_type(pctx, t))), tl)));
				case Too_many_arguments: "Too many arguments";
				case Could_not_unify(err): error_msg(err);
				case Cannot_skip_non_nullable(s): ^("Cannot skip non - nullable argument ", s);
			};

				public static function pass_name(match) return switch (match) {
				case PBuildModule: "build - module";
				case PBuildClass: "build - class";
					case PTypeField: "type - field";
					case PCheckConstraint: "check - constraint";
					case PForce: "force";
					case PFinal: "final";
				};

					public static function display_error(ctx, msg, p) return {
					ctx.on_error(ctx, msg, p);
				};

					public static function error(msg, p) return {
					raise(Error(Custom(msg), p));
				};

					public static function make_call(ctx, e, el, t, p) return {
					make_call_ref.val(ctx, e, el, t, p);
				};

					public static function type_expr(ctx, e, with_type) return {
					type_expr_ref.val(ctx, e, with_type);
				};

					public static function unify_min(ctx, el) return {
					unify_min_ref.val(ctx, el);
				};

					public static function match_expr(ctx, e, cases, def, with_type, p) return {
					match_expr_ref.val(ctx, e, cases, def, with_type, p);
				};

					public static function make_static_call(ctx, c, cf, map, args, t, p) return {
					var ta = TAnon({ () with a_fields = c.cl_statics;
					a_status = ref(Statics(c)) });
					var ethis = mk(TTypeExpr(TClassDecl(c)), ta, p);
					var monos = List.map(function _: mk_mono([]), cf.cf_params);
					function map(t) return {
					map(apply_params(cf.cf_params, monos, t));
				};
					var ef = mk(TField(ethis, FStatic(c, cf)), map(cf.cf_type), p);
					make_call(ctx, ef, args, map(t), p);
				};

					public static function unify(ctx, t1, t2, p) return {
					try {
					Type.unify(t1, t2);
				} catch(e:Unify_error(l)) {
					if (!(ctx.untyped)) {
					display_error(ctx, error_msg(Unify(l)), p);
				} else {
					[];
				};
				};
				};

					public static function unify_raise(ctx, t1, t2, p) return {
					try {
					Type.unify(t1, t2);
				} catch(e:Unify_error(l)) {
					raise(Error(Unify(l), p));
				};
				};

					public static function save_locals(ctx) return {
					var locals = ctx.locals;
					function []: ctx.locals = locals;
				};

					public static function add_local(ctx, n, t) return {
					var v = alloc_var(n, t);
					ctx.locals = PMap.add(n, v, ctx.locals);
					v;
				};

					public static var gen_local_prefix = "`";

					public static function gen_local(ctx, t) return {
					function loop(n) return {
					var nv = if (=(n, 0)) {
					gen_local_prefix;
				} else {
					^(gen_local_prefix, string_of_int(n));
				};
					if (PMap.mem(nv, ctx.locals)) {
					loop(+(n, 1));
				} else {
					nv;
				};
				};
					add_local(ctx, loop(0), t);
				};

					public static function is_gen_local(v) return {
					=(String.unsafe_get(v.v_name, 0), String.unsafe_get(gen_local_prefix, 0));
				};

					public static var not_opened = ref(Closed);

					public static function mk_anon(fl) return {
					TAnon({ () with a_fields = fl;
					a_status = not_opened });
				};

					public static function delay(ctx, p, f) return {
					function loop(match) return switch (match) {
					case []: ::((new Tuple(p, ::(f, []))), []);
					case ::((p2, l), rest): if (=(p2, p)) {
					::((new Tuple(p, ::(f, l))), rest);
				} else {
					if (<(p2, p)) {
					::((new Tuple(p2, l)), loop(rest));
				} else {
					::((new Tuple(p, ::(f, []))), ::((new Tuple(p2, l)), rest));
				};
				};
				};
					ctx.g.delayed = loop(ctx.g.delayed);
				};

					public static function flush_pass(ctx, p, wherestring) return {
					switch (ctx.g.delayed) {
					case ::((p2, l), rest) if(<=(p2, p)): switch (l) {
					case []: ctx.g.delayed = rest;
					case ::(f, l): ctx.g.delayed = ::((new Tuple(p2, l)), rest);
					f([]);
				};
					flush_pass(ctx, p, where);
					case _: [];
				};
				};

					public static function make_pass(ctx, f) return {
					f;
				};

					public static function init_class_done(ctx) return {
					ctx.pass = PTypeField;
				};

					public static function exc_protect(ctx, f, wherestring) return {
					var r = ref(function []: try {
					f(r);
				} catch(e:Error(m)(p)) {
					raise(Fatal_error(error_msg(m), p));
				});
					r;
				};

					public static var fake_modules = Hashtbl.create(0);

					public static function create_fake_module(ctx, file) return {
					var file = Common.unique_full_path(file);
					var mdep = try {
					Hashtbl.find(fake_modules, file);
				} catch(e:Not_found) {
					var mdep = { () with m_id = alloc_mid([]);
					m_path = (new Tuple(::("$DEP", []), file));
					m_types = [];
					m_extra = module_extra(file, Common.get_signature(ctx.com), file_time(file), MFake) };
					Hashtbl.add(fake_modules, file, mdep);
					mdep;
				};
					Hashtbl.replace(ctx.g.modules, mdep.m_path, mdep);
					mdep;
				}
				}
					;
