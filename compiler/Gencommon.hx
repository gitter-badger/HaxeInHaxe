import Unix;
import Ast;
import Type;
import Common;
import Option;
import Printf;
import ExtString;
import ExprHashtblHelper;
import ClosuresToClass;

enum Priority {
	PFirst;
	PLast;
	PZero;
	PCustom(value: Float);
};

class /*exception*/ DuplicateName {

};

class /*exception*/ NoRulesApplied {

};

class Rule_dispatchertpret {
	function name -> function ignore_not_found ->
	/*object self*/
	public var tbl = Hashtbl.create(16);
	public var keys = [];
	public var names = Hashtbl.create(16);
	public var temp = 0;
	public function add(nameoption(string), ? : (prioritypriority = PZero), ruletp -> option(ret)) return {
		var p = switch (priority) {
		case PFirst: infinity;
		case PLast: neg_infinity;
		case PZero: 0.0;
		case PCustom(i): i;
		};
		var q = if (!(Hashtbl.mem(tbl, p))) {
			var q = Stack.create([]);
			Hashtbl.add(tbl, p, q);
			keys = ::(p, keys);
			keys = List.sort(function x: function y: ~ -(compare(x, y)), keys);
			q;
		} else {
			Hashtbl.find(tbl, p);
		};
		var name = switch (name) {
		case None: temp = +(temp, 1);
			^ ("$_", string_of_int(temp));
		case Some(s): s;
		};
		if (Hashtbl.mem(names, name)) {
			raise(DuplicateName(name));
		} else {
			[];
		};
		Hashtbl.add(names, name, q);
		Stack.push((new Tuple(name, rule)), q);
	};
	public function describe() return {
		Hashtbl.iter(function s: function _: trace(s), names);
	};
	public function remove(namestring) return {
		if (Hashtbl.mem(names, name)) {
			var q = Hashtbl.find(names, name);
			var q_temp = Stack.create([]);
		Stack.iter(function case (n, _) if (=(n, name)): [];
			case _ = r: Stack.push(r, q_temp), q);
			Stack.clear(q);
			Stack.iter(function r: Stack.push(r, q), q_temp);
			Hashtbl.remove(names, name);
			True;
		} else {
			False;
		};
	};
	public function run_f(tp) return {
		get(selfrun(tp));
	};
	public function did_run(tp) return {
		is_some(selfrun(tp));
	};
	public function get_list() return {
		var ret = ref([]);
		List.iter(function key: var q = Hashtbl.find(tbl, key);
		Stack.iter(function (_, rule): ret.val = ::(rule, ret.val), q), keys);
		List.rev(ret.val);
	};
	public function run_from(priorityfloat, tptp) return {
		var ok = ref(ignore_not_found);
		var ret = ref(None);
		indent.val = ::("\t", indent.val);
		try {
			List.iter(function key:
			if ( < (key, priority)) {
			var q = Hashtbl.find(tbl, key);
				Stack.iter(function (n, rule): var t = if (debug_mode.val) {
				Common.timer( ^ ("rule dispatcher rule: ", n));
				} else {
					function []: [];
				};
				var r = rule(tp);
						t([]);
				if (is_some(r)) {
				ret.val = r;
				raise(Exit);
				} else {
					[];
				}, q);
			} else {
				[];
			}, keys);
		} catch (e: Exit) {
			ok.val = True;
		};
		switch (indent.val) {
		case []: [];
		case ::(h, t): indent.val = t;
		};
		if (!(ok.val)) {
			raise(NoRulesApplied);
		} else {
			[];
		};
		ret.val : option(ret);
	};
	public function run(tptp) return {
		selfrun_from(infinity, tp) : option(ret);
	}
};

class Rule_map_dispatchertp {
	function name ->
	/*object self*/
	inherit(!) ? ce (as s) ? nullrule_dispatchertptp(name)(True)super;
	public function run_f(tp) return {
		get(selfrun(tp));
	};
	public function run_from(priorityfloat, tptp) return {
		var cur = ref(tp);
		try {
			List.iter(function key:
			if ( < (key, priority)) {
			var q = Hashtbl.find(tbl, key);
				Stack.iter(function (n, rule): trace( ^ ("running rule ", n));
				var t = if (debug_mode.val) {
				Common.timer( ^ ("rule map dispatcher rule: ", n));
				} else {
					function []: [];
				};
				var r = rule(cur.val);
						t([]);
				if (is_some(r)) {
				cur.val = get(r);
				} else {
					[];
				}, q);
			} else {
				[];
			}, keys);
		} catch (e: Exit) {
			[];
		};
		Some(cur.val) : option(ret);
	}
};

typedef Generator_ctx = {
	gcon : Common.Context,
	gclasses : Gen_classes,
	gtools : Gen_tools,
	gmk_internal_name : String -> String -> String,
	gmodule_filters : Rule_map_dispatcher<Module_type>,
	gexpr_filters : Rule_map_dispatcher<Texpr>,
	gsyntax_filters : Rule_map_dispatcher<Texpr>,
	gfollow : Rule_dispatcher<T, T>,
	gtypes : Hashtbl<Path, Module_type>,
	gtypes_list : List<Module_type>,
	gmodules : List<Type.Module_def>,
	greal_field_types : Hashtbl<Tuple<Path, String>, Tuple<Tclass_field, T, T, Tclass>>,
	ghandle_cast : T -> T -> Texpr -> Texpr,
	gon_unsafe_cast : T -> T -> Pos -> Unit,
	gneeds_box : T -> Bool,
	gspecial_needs_cast : T -> T -> Bool,
	gsupported_conversions : Hashtbl < Path, T -> T -> Bool >,
	gadd_type : Module_type -> Bool -> Unit,
	gadd_to_module : Module_type -> Float -> Unit,
	gcurrent_path : Path,
	gcurrent_class : Option<Tclass>,
	gcurrent_classfield : Option<Tclass_field>,
	gon_classfield_start : List < Unit -> Unit >,
	gon_new_module_type : List < Unit -> Unit >,
	gafter_mod_filters_ended : List < Unit -> Unit >,
	gafter_expr_filters_ended : List < Unit -> Unit >,
	gafter_filters_ended : List < Unit -> Unit >,
	gbase_class_fields : PMap<String, Tclass_field>,
	greal_type : T -> T,
	greal_type_param : Module_type -> Tparams -> Tparams,
	gallow_tp_dynamic_conversion : Bool,
	guse_tp_constraints : Bool,
	gparam_func_call : Texpr -> Texpr -> Tparams -> List<Texpr> -> Texpr,
	ghas_tparam_cast_handler : Bool,
	gtparam_cast : Hashtbl < Path, Texpr -> T -> Texpr >,
	gspecial_vars : Hashtbl<String, Bool>
};

typedef Gen_classes = {
	cl_reflect : Tclass,
	cl_type : Tclass,
	cl_dyn : Tclass,
	t_iterator : Tdef,
	nativearray_len : Texpr -> Pos -> Texpr,
	nativearray_type : Type -> Type,
	nativearray : Type -> Type
};

typedef Gen_tools = {
	r_create_empty : Texpr -> T -> Texpr,
	r_fields : Bool -> Texpr -> Texpr,
	r_set_field : T -> Texpr -> Texpr -> Texpr -> Texpr,
	r_field : Bool -> T -> Texpr -> Texpr -> Texpr,
	rf_create_empty : Tclass -> Tparams -> Pos -> Texpr
};

enum Tfield_access {
	FClassField(value: TclassTparamsTclassTclass_fieldBoolTT);
	FEnumField(value: TenumTenum_fieldBool);
	FAnonField(value: Tclass_field);
	FDynamicField(value: T);
	FNotFound;
};

enum T_dependency {
	DAfter(value: Float);
	DBefore(value: Float);
};

class /*exception*/ ImpossibleDependency {

};

class /*exception*/ TypeNotFound {

};
class Gencommon {
	public static function debug_type_ctor(match) return switch (match) {
	case TMono(_): "TMono";
	case TEnum(_): "TEnum";
	case TInst(_): "TInst";
	case TType(_): "TType";
	case TFun(_): "TFun";
	case TAnon(_): "TAnon";
	case TDynamic(_): "TDynamic";
	case TLazy(_): "TLazy";
	case TAbstract(_): "TAbstract";
	};

	public static var debug_type = s_type(print_context([]));

	public static var debug_expr = s_expr(debug_type);

	public static function like_float(t) return {
		switch (follow(t)) {
		case TAbstract({ a_path = ([], Float) }, []) | TAbstract({ a_path = ([], Int) }, []): True;
		case TAbstract({ a_path = (::(cs, []), Pointer) }, _): False;
		case TAbstract(a, _):
			|| (List.exists(function t: like_float(t), a.a_from), List.exists(function t: like_float(t), a.a_to));
		case _: False;
		};
	};

	public static function like_int(t) return {
		switch (follow(t)) {
		case TAbstract({ a_path = ([], Int) }, []): True;
		case TAbstract({ a_path = (::(cs, []), Pointer) }, _): False;
		case TAbstract(a, _): || (List.exists(function t: like_int(t), a.a_from), List.exists(function t: like_int(t), a.a_to));
		case _: False;
		};
	};

	public static function like_i64(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = (::(cs, []), Int64) }, []) | TAbstract({ a_path = (::(cs, []), Int64) }, []) | TInst({ cl_path = (::(cs, []), UInt64) }, []) | TInst({ cl_path = (::(java, []), Int64) }, []) | TAbstract({ a_path = (::(java, []), Int64) }, []) | TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = (::(haxe, []), Int64) }, [])
			: True;
		case TAbstract(a, _): || (List.exists(function t: like_i64(t), a.a_from), List.exists(function t: like_i64(t), a.a_to));
		case _: False;
		};
	};

	public static function follow_once(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case Some(t): t;
			case _: t_dynamic;
			};
		case TLazy(f): f.val([]);
		case TType(t, tl): apply_params(t.t_params, tl, t.t_type);
		case _: t;
		};
	};

	public static var t_empty = TAnon({ () with a_fields = PMap.empty;
										a_status = ref(Closed)
									  });

	public static var tmp_count = ref(0);

	public static function reset_temps([]) return {
		tmp_count.val = 0;
	};

	public static var v_undefined = alloc_var("__undefined__", t_dynamic);

	public static function undefined(pos) return {
		{
			() with eexpr = TLocal(v_undefined);
			etype = t_dynamic;
			epos = pos
		};
	};

	public static function path_of_md_def(md_def) return {
		switch (md_def.m_types) {
		case ::(TClassDecl(c), []): c.cl_path;
		case _: md_def.m_path;
		};
	};

	public static var assertions = False;

	public static var debug_mode = ref(False);

	public static function trace(s) return {
		if (debug_mode.val) {
			print_endline(s);
		} else {
			[];
		};
	};

	public static function timer(name) return {
		if (debug_mode.val) {
			Common.timer(name);
		} else {
			function []: [];
		};
	};

	public static function is_string(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = ([], String) }, []): True;
		case _: False;
		};
	};

	public static function anon_of_classtype(cl) return {
		TAnon({
			() with a_fields = cl.cl_statics;
			a_status = ref(Statics(cl))
		});
	};

	public static function anon_of_enum(e) return {
		TAnon({
			() with a_fields = PMap.empty;
			a_status = ref(EnumStatics(e))
		});
	};

	public static function anon_of_abstract(a) return {
		TAnon({
			() with a_fields = PMap.empty;
			a_status = ref(AbstractStatics(a))
		});
	};

	public static function anon_of_mt(mt) return {
		switch (mt) {
		case TClassDecl(cl): anon_of_classtype(cl);
		case TEnumDecl(e): anon_of_enum(e);
		case TAbstractDecl(a): anon_of_abstract(a);
		case _: assert False;
		};
	};

	public static function anon_class(t) return {
		switch (follow(t)) {
		case TAnon(anon): switch (anon.a_status.val) {
			case Statics(cl): Some(TClassDecl(cl));
			case EnumStatics(e): Some(TEnumDecl(e));
			case AbstractStatics(a): Some(TAbstractDecl(a));
			case _: None;
			};
		case _: None;
		};
	};

	public static function path_s(path) return {
		switch (path) {
		case ([], s): s;
		case (p, s): ^ (String.concat(".", fst(path)), ^ (".", snd(path)));
		};
	};

	public static function t_to_md(t) return {
		switch (t) {
		case TInst(cl, _): TClassDecl(cl);
		case TEnum(e, _): TEnumDecl(e);
		case TType(t, _): TTypeDecl(t);
		case TAbstract(a, _): TAbstractDecl(a);
		case TAnon(anon): switch (anon.a_status.val) {
			case EnumStatics(e): TEnumDecl(e);
			case Statics(cl): TClassDecl(cl);
			case AbstractStatics(a): TAbstractDecl(a);
			case _: assert False;
			};
		case TLazy(f): t_to_md(f.val([]));
		case TMono(r): switch (r.val) {
			case Some(t): t_to_md(t);
			case None: assert False;
			};
		case _: assert False;
		};
	};

	public static function get_cl(mt) return {
		switch (mt) {
		case TClassDecl(cl): cl;
		case _: failwith( ^ ("Unexpected module type of '", ^ (path_s(t_path(mt)), "'")));
		};
	};

	public static function get_abstract(mt) return {
		switch (mt) {
		case TAbstractDecl(a): a;
		case _: failwith( ^ ("Unexpected module type of '", ^ (path_s(t_path(mt)), "'")));
		};
	};

	public static function get_tdef(mt) return {
		switch (mt) {
		case TTypeDecl(t): t;
		case _: assert False;
		};
	};

	public static function mk_mt_access(mt, pos) return {
		{
			() with eexpr = TTypeExpr(mt);
			etype = anon_of_mt(mt);
			epos = pos
		};
	};

	public static function is_void(t) return {
		switch (follow(t)) {
		case TAbstract({ a_path = ([], Void) }, []): True;
		case _: False;
		};
	};

	public static function mk_local(var, pos) return {
		{
			() with eexpr = TLocal(var);
			etype = var.v_type;
			epos = pos
		};
	};

	public static function get_fun(t) return {
		switch (follow(t)) {
		case TFun(r1, r2): (new Tuple(r1, r2));
		case _: trace(s_type(print_context([]), follow(t)));
			assert False;
		};
	};

	public static function mk_cast(t, e) return {
		{
			() with eexpr = TCast(e, None);
			etype = t;
			epos = e.epos
		};
	};

	public static function mk_classtype_access(cl, pos) return {
		{
			() with eexpr = TTypeExpr(TClassDecl(cl));
			etype = anon_of_classtype(cl);
			epos = pos
		};
	};

	public static function mk_static_field_access_infer(cl, field, pos, params) return {
		try {
			var cf = PMap.find(field, cl.cl_statics);
			{
				() with eexpr = TField(mk_classtype_access(cl, pos), FStatic(cl, cf));
				etype = if ( = (params, [])) {
					cf.cf_type;
				} else {
					apply_params(cf.cf_params, params, cf.cf_type);
				};
				epos = pos
			};
		} catch (e: Not_found) {
			failwith( ^ ("Cannot find field ", ^ (field, ^ (" in type ", path_s(cl.cl_path)))));
		};
	};

	public static function mk_static_field_access(cl, field, fieldt, pos) return {
		{ (mk_static_field_access_infer(cl, field, pos, [])) with etype = fieldt };
	};

	public static var indent = ref([]);

	public static function get_type(types, path) return {
		List.find(function md:
		switch (md) {
	case TClassDecl(cl) if (=(cl.cl_path, path)): True;
		case TEnumDecl(e) if (=(e.e_path, path)): True;
		case TTypeDecl(t) if (=(t.t_path, path)): True;
		case TAbstractDecl(a) if (=(a.a_path, path)): True;
		case _: False;
		}, types);
	};

	public static function new_ctx(con) return {
		var types = Hashtbl.create(List.length(con.types));
		List.iter(function mt:
		switch (mt) {
	case TClassDecl(cl): Hashtbl.add(types, cl.cl_path, mt);
		case TEnumDecl(e): Hashtbl.add(types, e.e_path, mt);
		case TTypeDecl(t): Hashtbl.add(types, t.t_path, mt);
		case TAbstractDecl(a): Hashtbl.add(types, a.a_path, mt);
		}, con.types);
		var cl_dyn = switch (get_type(con.types, (new Tuple([], "Dynamic")))) {
		case TClassDecl(c): c;
		case TAbstractDecl(a): mk_class(a.a_module, (new Tuple([], "Dynamic")), a.a_pos);
		case _: assert False;
		};
		var gen = { () with gcon = con;
					gclasses = { () with cl_reflect = get_cl(get_type(con.types, (new Tuple([], "Reflect"))));
								 cl_type = get_cl(get_type(con.types, (new Tuple([], "Type"))));
								 cl_dyn = cl_dyn;
								 t_iterator = get_tdef(get_type(con.types, (new Tuple([], "Iterator"))));
								 nativearray = function _: assert False;
								 nativearray_type = function _: assert False;
								 nativearray_len = function _: assert False
							   };
		gtools = { () with r_create_empty = function eclass: function t: var fieldcall = mk_static_field_access_infer(gen.gclasses.cl_type, "createEmptyInstance", eclass.epos, ::(t, []));
			{
				() with eexpr = TCall(fieldcall, ::(eclass, []));
				etype = t;
				epos = eclass.epos
			};
			r_fields = function is_used_only_by_iteration: function expr: var fieldcall = mk_static_field_access_infer(gen.gclasses.cl_reflect, "fields", expr.epos, []);
			{
				() with eexpr = TCall(fieldcall, ::(expr, []));
				etype = gen.gcon.basic.tarray(gen.gcon.basic.tstring);
				epos = expr.epos
			};
			r_set_field = function t: function obj: function field: function v: var fieldcall = mk_static_field_access_infer(gen.gclasses.cl_reflect, "setField", v.epos, []);
			{
				() with eexpr = TCall(fieldcall, ::(obj, ::(field, ::(v, []))));
				etype = t_dynamic;
				epos = v.epos
			};
			r_field = function is_safe: function t: function obj: function field: var fieldcall = mk_static_field_access_infer(gen.gclasses.cl_reflect, "field", obj.epos, []);
			mk_cast(t, { () with eexpr = TCall(fieldcall, ::(obj, ::(field, [])));
						 etype = t_dynamic;
						 epos = obj.epos
					   });
			rf_create_empty = function cl: function p: function pos: gen.gtools.r_create_empty({ () with eexpr = TTypeExpr(TClassDecl(cl));
							  epos = pos;
							  etype = t_dynamic
																							   }, TInst(cl, p))
		};
		gmk_internal_name = function ns: function s: sprintf("__%s_%s", ns, s);
							gexpr_filters = new rule_map_dispatcher("gexpr_filters");
							gmodule_filters = new rule_map_dispatcher("gmodule_filters");
							gsyntax_filters = new rule_map_dispatcher("gsyntax_filters");
							gfollow = new rule_dispatcher("gfollow", False);
							gtypes = types;
							gtypes_list = con.types;
							gmodules = con.modules;
							greal_field_types = Hashtbl.create(0);
							ghandle_cast = function to_t: function from_t: function e: mk_cast(to_t, e);
							gon_unsafe_cast = function t: function t2: function pos: gen.gcon.warning( ^ ("Type ", ^ (debug_type(t2), ^ (" is being cast to the unrelated type ", s_type(print_context([]), t)))), pos);
							gneeds_box = function t: False;
							gspecial_needs_cast = function to_t: function from_t: True;
							gsupported_conversions = Hashtbl.create(0);
							gadd_type = function md: function should_filter:
		if (should_filter) {
		gen.gtypes_list = ::(md, gen.gtypes_list);
			gen.gmodules = ::({ () with m_id = alloc_mid([]);
								m_path = t_path(md);
								m_types = ::(md, []);
								m_extra = module_extra("", "", 0., MFake)
							  }, gen.gmodules);
			Hashtbl.add(gen.gtypes, t_path(md), md);
		} else {
			gen.gafter_filters_ended = ::(function []: gen.gtypes_list = ::(md, gen.gtypes_list);
			gen.gmodules = ::({
				() with m_id = alloc_mid([]);
				m_path = t_path(md);
				m_types = ::(md, []);
				m_extra = module_extra("", "", 0., MFake)
			}, gen.gmodules);
			Hashtbl.add(gen.gtypes, t_path(md), md), gen.gafter_filters_ended);
		};
		gadd_to_module = function md: function pr: failwith("module added outside expr filters");
						 gcurrent_path = (new Tuple([], ""));
						 gcurrent_class = None;
						 gcurrent_classfield = None;
						 gon_classfield_start = [];
						 gon_new_module_type = [];
						 gafter_mod_filters_ended = [];
						 gafter_expr_filters_ended = [];
						 gafter_filters_ended = [];
						 gbase_class_fields = PMap.empty;
						 greal_type = function t: t;
						 greal_type_param = function _: function t: t;
						 gallow_tp_dynamic_conversion = False;
						 guse_tp_constraints = False;
						 gparam_func_call = function ecall: function efield: function params: function elist: { (ecall) with eexpr = TCall(efield, elist) };
						 ghas_tparam_cast_handler = False;
						 gtparam_cast = Hashtbl.create(0);
						 gspecial_vars = Hashtbl.create(0)
				  };
		gen;
	};

	public static function init_ctx(gen) return {
		var follow_f = gen.gfollowrun;
		function follow(t) return {
			switch (t) {
			case TMono(r): switch (r.val) {
				case Some(t): follow_f(t);
				case _: Some(t);
				};
			case TLazy(f): follow_f(f.val([]));
			case TType(t, tl): follow_f(apply_params(t.t_params, tl, t.t_type));
			case _: Some(t);
			};
		};
		gen.gfollowadd(name = "final", priority = PLast, follow);
	};

	public static function run_follow(gen) return {
		gen.gfollowrun_f;
	};

	public static function reorder_modules(gen) return {
		var modules = Hashtbl.create(20);
		List.iter(function md: Hashtbl.add(modules, t_infos(md).mt_module.m_path, md), gen.gtypes_list);
		gen.gmodules = [];
		var processed = Hashtbl.create(20);
		Hashtbl.iter(function md_path: function md:
		if (!(Hashtbl.mem(processed, md_path))) {
		Hashtbl.add(processed, md_path, True);
			gen.gmodules = ::({
				() with m_id = alloc_mid([]);
				m_path = md_path;
				m_types = List.rev(Hashtbl.find_all(modules, md_path));
				m_extra = t_infos(md).mt_module.m_extra
			}, gen.gmodules);
		} else {
			[];
		}, modules);
	};

	public static function run_filters_from(gen, t, filters) return {
		switch (t) {
		case TClassDecl(c): trace(snd(c.cl_path));
			gen.gcurrent_path = c.cl_path;
			gen.gcurrent_class = Some(c);
			List.iter(function fn: fn([]), gen.gon_new_module_type);
			gen.gcurrent_classfield = None;
			function process_field(f) return {
				reset_temps([]);
				gen.gcurrent_classfield = Some(f);
				List.iter(function fn: fn([]), gen.gon_classfield_start);
				trace(f.cf_name);
				switch (f.cf_expr) {
				case None: [];
				case Some(e): f.cf_expr = Some(List.fold_left(function e: function f: f(e), e, filters));
				};
				List.iter(process_field, f.cf_overloads);
			};
			List.iter(process_field, c.cl_ordered_fields);
			List.iter(process_field, c.cl_ordered_statics);
			switch (c.cl_constructor) {
			case None: [];
			case Some(f): process_field(f);
			};
			gen.gcurrent_classfield = None;
			switch (c.cl_init) {
			case None: [];
			case Some(e): c.cl_init = Some(List.fold_left(function e: function f: f(e), e, filters));
			};
		case TEnumDecl(_): [];
		case TTypeDecl(_): [];
		case TAbstractDecl(_): [];
		};
	};

	public static function run_filters(gen) return {
		var last_error = gen.gcon.error;
		var has_errors = ref(False);
		gen.gcon.error = function msg: function pos: has_errors.val = True;
		last_error(msg, pos);
		var t = Common.timer("gencommon_filters");
		if (Common.defined(gen.gcon, Define.GencommonDebug)) {
			debug_mode.val = True;
		} else {
			debug_mode.val = False;
		};
		function run_filters(filter) return {
			function loop(acc, mds) return {
				switch (mds) {
				case []: acc;
				case ::(md, tl): var filters = ::(filterrun_f, []);
					var added_types = ref([]);
					gen.gadd_to_module = function md_type: function priority: gen.gtypes_list = ::(md_type, gen.gtypes_list);
					added_types.val = ::((new Tuple(md_type, priority)), added_types.val);
					run_filters_from(gen, md, filters);
					var added_types = List.map(function (t, p): run_filters_from(gen, t, ::(function e: get(filterrun_from(p, e)), []));
					if (Hashtbl.mem(gen.gtypes, t_path(t))) {
					function loop(i) return {
							var p = t_path(t);
							var new_p = (new Tuple(fst(p), ^ (snd(p), ^ ("_", string_of_int(i)))));
							if (Hashtbl.mem(gen.gtypes, new_p)) {
								loop(+(i, 1));
							} else {
								switch (t) {
								case TClassDecl(cl): cl.cl_path = new_p;
								case TEnumDecl(e): e.e_path = new_p;
								case TTypeDecl(_) | TAbstractDecl(_): [];
								};
							};
						};
						loop(0);
					} else {
						[];
					};
					Hashtbl.add(gen.gtypes, t_path(t), t);
					t, added_types.val);
					loop(@(added_types, ::(md, acc)), tl);
				};
			};
			List.rev(loop([], gen.gtypes_list));
		};
		function run_mod_filter(filter) return {
			var last_add_to_module = gen.gadd_to_module;
			var added_types = ref([]);
			gen.gadd_to_module = function md_type: function priority: Hashtbl.add(gen.gtypes, t_path(md_type), md_type);
			added_types.val = ::((new Tuple(md_type, priority)), added_types.val);
			function loop(processed, not_processed) return {
				switch (not_processed) {
				case ::(hd, tl): switch (hd) {
					case TClassDecl(c): gen.gcurrent_class = Some(c);
					case _: gen.gcurrent_class = None;
					};
					var new_hd = filterrun_f(hd);
					var added_types_new = added_types.val;
					added_types.val = [];
					var added_types = List.map(function (t, p): get(filterrun_from(p, t)), added_types_new);
					loop(@(added_types, ::(new_hd, processed)), tl);
				case []: processed;
				};
			};
			var filtered = loop([], gen.gtypes_list);
			gen.gadd_to_module = last_add_to_module;
			gen.gtypes_list = List.rev(filtered);
		};
		run_mod_filter(gen.gmodule_filters);
		List.iter(function fn: fn([]), gen.gafter_mod_filters_ended);
		var last_add_to_module = gen.gadd_to_module;
		gen.gtypes_list = run_filters(gen.gexpr_filters);
		gen.gadd_to_module = last_add_to_module;
		List.iter(function fn: fn([]), gen.gafter_expr_filters_ended);
		gen.gtypes_list = run_filters(gen.gsyntax_filters);
		List.iter(function fn: fn([]), gen.gafter_filters_ended);
		reorder_modules(gen);
		t([]);
		if (has_errors.val) {
			raise(Abort("Compilation aborted with errors", null_pos));
		} else {
			[];
		};
	};

	public static function write_file(gen, w, source_dir, path, extension, out_files) return {
		var t = timer("write file");
		var s_path = ^ (source_dir, ^ ("/", ^ (snd(path), ^ (".", extension))));
		mkdir_from_path(s_path);
		var contents = SourceWriter.contents(w);
		var should_write = if ( && (!(Common.defined(gen.gcon, Define.ReplaceFiles)), Sys.file_exists(s_path))) {
			var in_file = open_in(s_path);
			var old_contents = Std.input_all(in_file);
			close_in(in_file);
			<>(contents, old_contents);
		} else {
			True;
		};
		if (should_write) {
			var f = open_out_bin(s_path);
			output_string(f, contents);
			close_out(f);
		} else {
			[];
		};
		out_files.val = ::(unique_full_path(s_path), out_files.val);
		t([]);
	};

	public static function clean_files(path, excludes, verbose) return {
		function iter_files(pack, dir, path) return {
			try {
				var file = Unix.readdir(dir);
				if ( && (<>(file, "."), <>(file, ".."))) {
					var filepath = ^ (path, ^ ("/", file));
					if ( = (Unix.stat(filepath).st_kind, S_DIR)) {
						var pack = @(pack, ::(file, []));
						iter_files(pack, Unix.opendir(filepath), filepath);
						try {
							Unix.rmdir(filepath);
						} catch (e: Unix.Unix_error(ENOTEMPTY)(_)(_)) {
							[];
						};
					} else {
						if ( && (!(String.ends_with(filepath, ".meta")), !(List.mem(unique_full_path(filepath), excludes)))) {
							if (verbose) {
								print_endline( ^ ("Removing ", filepath));
							} else {
								[];
							};
							Sys.remove(filepath);
						} else {
							[];
						};
					};
				} else {
					[];
				};
				iter_files(pack, dir, path);
			} catch (e: End_of_file | Unix.Unix_error(_)) {
				Unix.closedir(dir);
			};
		};
		iter_files([], Unix.opendir(path), path);
	};

	public static function dump_descriptor(gen, name, path_s, module_s) return {
		var w = SourceWriter.new_source_writer([]);
		SourceWriter.write(w, Sys.getcwd([]));
		SourceWriter.newline(w);
		SourceWriter.write(w, "begin defines");
		SourceWriter.newline(w);
		PMap.iter(function name: function _: SourceWriter.write(w, name);
		SourceWriter.newline(w), gen.gcon.defines);
		SourceWriter.write(w, "end defines");
		SourceWriter.newline(w);
		SourceWriter.write(w, "begin defines_data");
		SourceWriter.newline(w);
		PMap.iter(function name: function v: SourceWriter.write(w, name);
		SourceWriter.write(w, "=");
		SourceWriter.write(w, v);
		SourceWriter.newline(w), gen.gcon.defines);
		SourceWriter.write(w, "end defines_data");
		SourceWriter.newline(w);
		SourceWriter.write(w, "begin modules");
		SourceWriter.newline(w);
		var main_paths = Hashtbl.create(0);
		List.iter(function md_def: SourceWriter.write(w, "M ");
		SourceWriter.write(w, path_s(path_of_md_def(md_def)));
		SourceWriter.newline(w);
		List.iter(function m:
		switch (m) {
	case TClassDecl(cl) if (!(cl.cl_extern)): SourceWriter.write(w, "C ");
			var s = module_s(m);
			Hashtbl.add(main_paths, cl.cl_path, s);
			SourceWriter.write(w, s);
			SourceWriter.newline(w);
		case TEnumDecl(e) if (!(e.e_extern)): SourceWriter.write(w, "E ");
			SourceWriter.write(w, module_s(m));
			SourceWriter.newline(w);
		case _: [];
		}, md_def.m_types), gen.gmodules);
		SourceWriter.write(w, "end modules");
		SourceWriter.newline(w);
		switch (gen.gcon.main_class) {
		case Some(path): SourceWriter.write(w, "begin main");
			SourceWriter.newline(w);
			try {
				SourceWriter.write(w, Hashtbl.find(main_paths, path));
			} catch (e: Not_found) {
				SourceWriter.write(w, path_s(path));
			};
			SourceWriter.newline(w);
			SourceWriter.write(w, "end main");
			SourceWriter.newline(w);
		case _: [];
		};
		SourceWriter.write(w, "begin resources");
		SourceWriter.newline(w);
		Hashtbl.iter(function name: function _: SourceWriter.write(w, name);
					 SourceWriter.newline(w), gen.gcon.resources);
		SourceWriter.write(w, "end resources");
		SourceWriter.newline(w);
		SourceWriter.write(w, "begin libs");
		SourceWriter.newline(w);
		function path(file, ext) return {
			if (Sys.file_exists(file)) {
				file;
			} else {
				try {
					Common.find_file(gen.gcon, file);
				} catch (e: Not_found) {
					try {
						Common.find_file(gen.gcon, ^ (file, ext));
					} catch (e: Not_found) {
						file;
					};
				};
			};
		};
		if (Common.platform(gen.gcon, Java)) {
			List.iter(function (s, std, _, _, _):
			if (!(std)) {
			SourceWriter.write(w, path(s, ".jar"));
				SourceWriter.newline(w);
			} else {
				[];
			}, gen.gcon.java_libs);
		} else {
			if (Common.platform(gen.gcon, Cs)) {
				List.iter(function (s, std, _, _):
				if (!(std)) {
				SourceWriter.write(w, path(s, ".dll"));
					SourceWriter.newline(w);
				} else {
					[];
				}, gen.gcon.net_libs);
			} else {
				[];
			};
		};
		SourceWriter.write(w, "end libs");
		SourceWriter.newline(w);
		var args = gen.gcon.c_args;
		if (<>(args, [])) {
			SourceWriter.write(w, "begin opts");
			SourceWriter.newline(w);
			List.iter(function opt: SourceWriter.write(w, opt);
					  SourceWriter.newline(w), List.rev(args));
			SourceWriter.write(w, "end opts");
			SourceWriter.newline(w);
		} else {
			[];
		};
		var contents = SourceWriter.contents(w);
		var f = open_out( ^ (gen.gcon.file, ^ ("/", name)));
		output_string(f, contents);
		close_out(f);
	};

	public static var path_regex = Str.regexp("[/\\]+");

	public static function normalize(path) return {
		function normalize(acc, m) return {
			switch (m) {
			case []: List.rev(acc);
			case ::(Str.Text(.), ::(Str.Delim(_), tl)) if (=(acc, [])): normalize([], tl);
			case ::(Str.Text(..), ::(Str.Delim(_), tl)): switch (acc) {
				case []: raise(Exit);
				case ::(_, acc): normalize(acc, tl);
				};
			case ::(Str.Text(t), ::(Str.Delim(_), tl)): normalize(::(t, acc), tl);
			case ::(Str.Delim(_), tl): normalize(::("", acc), tl);
			case ::(Str.Text(t), []): List.rev(::(t, acc));
			case ::(Str.Text(_), ::(Str.Text(_), _)): assert False;
			};
		};
		String.concat("/", normalize([], Str.full_split(path_regex, path)));
	};

	public static function is_relative(cwd, rel) return {
		try {
			var rel = normalize(rel);
			|| (Filename.is_relative(rel), || (String.starts_with(rel, cwd), String.starts_with(Common.unique_full_path(rel), cwd)));
		} catch (e: Exit) {
			|| (String.starts_with(rel, cwd), String.starts_with(Common.unique_full_path(rel), cwd));
		};
	};

	public static function generate_modules(gen, extension, source_dir,
											module_genSourceWriter.source_writer -> module_def -> bool, out_files) return {
		List.iter(function md_def: var source_dir = ^ (gen.gcon.file, ^ ("/", ^ (source_dir, ^ ("/", String.concat("/", fst(path_of_md_def(md_def)))))));
		var w = SourceWriter.new_source_writer([]);
		var should_write = module_gen(w, md_def);
		if (should_write) {
		var path = path_of_md_def(md_def);
			write_file(gen, w, source_dir, path, extension, out_files);
		} else {
			[];
		}, gen.gmodules);
	};

	public static function generate_modules_t(gen, extension, source_dir, change_path,
			module_genSourceWriter.source_writer -> module_type -> bool, out_files) return {
		var source_dir = ^ (gen.gcon.file, ^ ("/", source_dir));
		List.iter(function md: var w = SourceWriter.new_source_writer([]);
		var should_write = module_gen(w, md);
		if (should_write) {
		var path = change_path(t_path(md));
			write_file(gen, w, ^ (source_dir, ^ ("/", String.concat("/", fst(path)))), path, extension, out_files);
		} else {
			[];
		}, gen.gtypes_list);
	};

	public static function mk_paren(e) return {
		switch (e.eexpr) {
		case TParenthesis(_): e;
		case _: {
			(e) with eexpr = TParenthesis(e)
		};
		};
	};

	public static function get_real_fun(gen, t) return {
		switch (follow(t)) {
		case TFun(args, t): TFun(List.map(function (n, o, t): (new Tuple(n, o, gen.greal_type(t))), args), gen.greal_type(t));
		case _: t;
		};
	};

	public static function mk_int(gen, i, pos) return {
		{
			() with eexpr = TConst(TInt(Int32.of_int(i)));
			etype = gen.gcon.basic.tint;
			epos = pos
		};
	};

	public static function mk_return(e) return {
		{
			() with eexpr = TReturn(Some(e));
			etype = e.etype;
			epos = e.epos
		};
	};

	public static function mk_temp(gen, name, t) return {
		incr(tmp_count);
		var name = gen.gmk_internal_name("temp", ^ (name, string_of_int(tmp_count.val)));
		alloc_var(name, t);
	};

	public static var v_nativearray = alloc_var("__array__", t_dynamic);

	public static function mk_nativearray_decl(gen, t, el, pos) return {
		{
			() with eexpr = TCall(mk_local(v_nativearray, pos), el);
			etype = gen.gclasses.nativearray(t);
			epos = pos
		};
	};

	public static function ensure_local(gen, block, name, e) return {
		switch (e.eexpr) {
		case TLocal(_): e;
		case _: var var = mk_temp(gen, name, e.etype);
			block.val = ::({
				(e) with eexpr = TVar(var, Some(e));
				etype = gen.gcon.basic.tvoid
			}, block.val);
			{
				(e) with eexpr = TLocal(var)
			};
		};
	};

	public static function follow_module(follow_func, md) return {
		switch (md) {
		case TClassDecl(_) | TEnumDecl(_) | TAbstractDecl(_): md;
		case TTypeDecl(tdecl): switch (follow_func(TType(tdecl, List.map(snd, tdecl.t_params)))) {
			case TInst(cl, _): TClassDecl(cl);
			case TEnum(e, _): TEnumDecl(e);
			case TType(t, _): TTypeDecl(t);
			case TAbstract(a, _): TAbstractDecl(a);
			case _: assert False;
			};
		};
	};

	public static function is_hxgen(md) return {
		switch (md) {
		case TClassDecl(cl): Meta.has(Meta.HxGen, cl.cl_meta);
		case TEnumDecl(e): Meta.has(Meta.HxGen, e.e_meta);
		case TTypeDecl(t): || (Meta.has(Meta.HxGen, t.t_meta), switch (follow(t.t_type)) {
		case TInst(cl, _): is_hxgen(TClassDecl(cl));
			case TEnum(e, _): is_hxgen(TEnumDecl(e));
			case _: False;
			});
		case TAbstractDecl(a): Meta.has(Meta.HxGen, a.a_meta);
		};
	};

	public static function is_hxgen_t(t) return {
		switch (t) {
		case TInst(cl, _): Meta.has(Meta.HxGen, cl.cl_meta);
		case TEnum(e, _): Meta.has(Meta.HxGen, e.e_meta);
		case TAbstract(a, _): Meta.has(Meta.HxGen, a.a_meta);
		case TType(t, _): Meta.has(Meta.HxGen, t.t_meta);
		case _: False;
		};
	};

	public static function mt_to_t_dyn(md) return {
		switch (md) {
		case TClassDecl(cl): TInst(cl, List.map(function _: t_dynamic, cl.cl_params));
		case TEnumDecl(e): TEnum(e, List.map(function _: t_dynamic, e.e_params));
		case TAbstractDecl(a): TAbstract(a, List.map(function _: t_dynamic, a.a_params));
		case TTypeDecl(t): TType(t, List.map(function _: t_dynamic, t.t_params));
		};
	};

	public static function mt_to_t(mt, params) return {
		switch (mt) {
		case TClassDecl(cl): TInst(cl, params);
		case TEnumDecl(e): TEnum(e, params);
		case TAbstractDecl(a): TAbstract(a, params);
		case _: assert False;
		};
	};

	public static function t_to_mt(t) return {
		switch (follow(t)) {
		case TInst(cl, _): TClassDecl(cl);
		case TEnum(e, _): TEnumDecl(e);
		case TAbstract(a, _): TAbstractDecl(a);
		case _: assert False;
		};
	};

	public static function get_last_ctor(cl) return {
		Option.map_default(function (super, _):
		if (is_some(super.cl_constructor)) {
		Some(get(super.cl_constructor));
		} else {
			get_last_ctor(super);
		}, None, cl.cl_super);
	};

	public static function add_constructor(cl, cf) return {
		switch (cl.cl_constructor) {
		case None: cl.cl_constructor = Some(cf);
		case Some(ctor): if ( && ( != (ctor, cf), !(List.memq(cf, ctor.cf_overloads)))) {
				ctor.cf_overloads = ::(cf, ctor.cf_overloads);
			} else {
				[];
			};
		};
	};

	public static function replace_mono(t) return {
		switch (t) {
		case TMono(t): switch (t.val) {
			case None: t.val = Some(t_dynamic);
			case Some(_): [];
			};
		case TEnum(_, p) | TInst(_, p) | TType(_, p) | TAbstract(_, p): List.iter(replace_mono, p);
		case TFun(args, ret): List.iter(function (_, _, t): replace_mono(t), args);
			replace_mono(ret);
		case TAnon(_) | TDynamic(_): [];
		case TLazy(f): replace_mono(f.val([]));
		};
	};

	public static function mk_class_field(name, t, public, pos, kind, params) return {
		{
			() with cf_name = name;
			cf_type = t;
			cf_public = public;
			cf_pos = pos;
			cf_doc = None;
			cf_meta = ::((new Tuple(Meta.CompilerGenerated, [], Ast.null_pos)), []);
			cf_kind = kind;
			cf_params = params;
			cf_expr = None;
			cf_overloads = []
		};
	};

	public static function map_param(cl) return {
		var ret = mk_class(cl.cl_module, (new Tuple(fst(cl.cl_path), ^ (snd(cl.cl_path), "_c"))), cl.cl_pos);
		ret.cl_implements = cl.cl_implements;
		ret.cl_kind = cl.cl_kind;
		ret;
	};

	public static function get_cl_t(t) return {
		switch (follow(t)) {
		case TInst(cl, _): cl;
		case _: assert False;
		};
	};

	public static function mk_class(m, path, pos) return {
		var cl = Type.mk_class(m, path, pos);
		cl.cl_meta = ::((new Tuple(Meta.CompilerGenerated, [], Ast.null_pos)), []);
		cl;
	};

	public static function is_var(f) return {
		switch (f.cf_kind) {
		case Var(_): True;
		case _: False;
		};
	};

	public static function find_first_declared_field(gen, orig_cl, get_vmtype, exact_field, field) return {
		var get_vmtype = switch (get_vmtype) {
		case None: function t: t;
		case Some(f): f;
		};
		var chosen = ref(None);
		var is_overload = ref(False);
		function loop_cl(depth, c, tl, tlch) return {
			try {
				var ret = PMap.find(field, c.cl_fields);
				if (Meta.has(Meta.Overload, ret.cf_meta)) {
					is_overload.val = True;
				} else {
					[];
				};
				switch ((new Tuple(chosen.val, exact_field))) {
				case (Some(d, f, _, _, _), _) if (||(<=(depth, d), &&(is_var(ret), !(is_var(f))))): [];
				case (_, None): chosen.val = Some(depth, ret, c, tl, tlch);
				case (_, Some(f2)): List.iter(function f: var declared_t = apply_params(c.cl_params, tl, f.cf_type);
					if (Typeload.same_overload_args(get_vmtype = , declared_t, f2.cf_type, f, f2)) {
					chosen.val = Some(depth, f, c, tl, tlch);
					} else {
						[];
					}, ::(ret, ret.cf_overloads));
				};
			} catch (e: Not_found) {
				[];
			};
			switch (c.cl_super) {
			case Some(sup, stl): var tl = List.map(apply_params(c.cl_params, tl), stl);
				var stl = gen.greal_type_param(TClassDecl(sup), stl);
				var tlch = List.map(apply_params(c.cl_params, tlch), stl);
				loop_cl(+(depth, 1), sup, tl, tlch);
			case None: [];
			};
			if (c.cl_interface) {
				List.iter(function (sup, stl): var tl = List.map(apply_params(c.cl_params, tl), stl);
						  var stl = gen.greal_type_param(TClassDecl(sup), stl);
						  var tlch = List.map(apply_params(c.cl_params, tlch), stl);
						  loop_cl(+(depth, 1), sup, tl, tlch), c.cl_implements);
			} else {
				[];
			};
		};
		loop_cl(0, orig_cl, List.map(snd, orig_cl.cl_params), List.map(snd, orig_cl.cl_params));
		switch (chosen.val) {
		case None: None;
		case Some(_, f, c, tl, tlch): if ( && (is_overload.val, !(Meta.has(Meta.Overload, f.cf_meta)))) {
				f.cf_meta = ::((new Tuple(Meta.Overload, [], f.cf_pos)), f.cf_meta);
			} else {
				[];
			};
			var declared_t = apply_params(c.cl_params, tl, f.cf_type);
			var params_t = apply_params(c.cl_params, tlch, f.cf_type);
			var actual_t = switch (follow(params_t)) {
			case TFun(args, ret): TFun(List.map(function (n, o, t): (new Tuple(n, o, gen.greal_type(t))), args), gen.greal_type(ret));
			case _: gen.greal_type(params_t);
			};
			Some(f, actual_t, declared_t, params_t, c, tl, tlch);
		};
	};

	public static function field_access(gen, tt, fieldstring) return {
		var t = switch (gen.greal_type(t)) {
		case TAbstract({ a_path = (::(cs, []), Pointer) }, ::(t, [])): gen.greal_type(t);
		case _: t;
		};
		switch (follow(t)) {
		case TInst(cl, params): var orig_cl = cl;
			var orig_params = params;
			function not_found(cl, params) return {
				switch (cl.cl_dynamic) {
				case Some(t): var t = apply_params(cl.cl_params, params, t);
					FDynamicField(t);
				case None: switch (cl.cl_super) {
					case None: FNotFound;
					case Some(super, p): not_found(super, p);
					};
				};
			};
			function not_found([]) return {
				try {
					var cf = PMap.find(field, gen.gbase_class_fields);
					FClassField(orig_cl, orig_params, gen.gclasses.cl_dyn, cf, False, cf.cf_type, cf.cf_type);
				} catch (e: Not_found) {
					not_found(cl, params);
				};
			};
			var hashtbl_field = ^ (String.concat("", List.map(function _: "]", cl.cl_params)), field);
			var types = try {
				Hashtbl.find(gen.greal_field_types, (new Tuple(orig_cl.cl_path, hashtbl_field)));
			} catch (e: Not_found) {
				var ret = find_first_declared_field(gen, cl, field);
				var ret = switch (ret) {
				case None: None;
				case Some(cf, t, dt, _, cl, _, _): Some(cf, t, dt, cl);
				};
				if (<>(ret, None)) {
					Hashtbl.add(gen.greal_field_types, (new Tuple(orig_cl.cl_path, hashtbl_field)), ret);
				} else {
					[];
				};
				ret;
			};
			switch (types) {
			case None: not_found([]);
			case Some(cf, actual_t, declared_t, declared_cl): FClassField(orig_cl, orig_params, declared_cl, cf, False, actual_t,
						declared_t);
			};
		case TEnum(_) | TAbstract(_): FNotFound;
		case TAnon(anon): try {
				switch (anon.a_status.val) {
				case Statics(cl): var cf = PMap.find(field, cl.cl_statics);
					FClassField(cl, List.map(function _: t_dynamic, cl.cl_params), cl, cf, True, cf.cf_type, cf.cf_type);
				case EnumStatics(e): var f = PMap.find(field, e.e_constrs);
					var is_param = switch (follow(f.ef_type)) {
					case TFun(_): True;
					case _: False;
					};
					FEnumField(e, f, is_param);
				case _ if (PMap.mem(field, gen.gbase_class_fields)): var cf = PMap.find(field, gen.gbase_class_fields);
					FClassField(gen.gclasses.cl_dyn, ::(t_dynamic, []), gen.gclasses.cl_dyn, cf, False, cf.cf_type, cf.cf_type);
				case _: FAnonField(PMap.find(field, anon.a_fields));
				};
			} catch (e: Not_found) {
				FNotFound;
			};
		case _ if (PMap.mem(field, gen.gbase_class_fields)): var cf = PMap.find(field, gen.gbase_class_fields);
			FClassField(gen.gclasses.cl_dyn, ::(t_dynamic, []), gen.gclasses.cl_dyn, cf, False, cf.cf_type, cf.cf_type);
		case TDynamic(t): FDynamicField(t);
		case TMono(_): FDynamicField(t_dynamic);
		case _: FNotFound;
		} : tfield_access;
	};

	public static function field_access_esp(gen, t, field) return {
		switch (field) {
		case FStatic(cl, cf) | FInstance(cl, _, cf) if (Meta.has(Meta.Extern, cf.cf_meta)): var static = switch (field) {
			case FStatic(_): True;
			case _: False;
			};
			var p = switch (follow(run_follow(gen, t))) {
			case TInst(_, p): p;
			case _: List.map(snd, cl.cl_params);
			};
			FClassField(cl, p, cl, cf, static, cf.cf_type, cf.cf_type);
		case _: field_access(gen, t, field_name(field));
		};
	};

	public static function mk_field_access(gen, expr, field, pos) return {
		switch (field_access(gen, expr.etype, field)) {
		case FClassField(c, p, dc, cf, False, at, _): {
			() with eexpr = TField(expr, FInstance(dc, p, cf));
			etype = apply_params(c.cl_params, p, at);
			epos = pos
		};
		case FClassField(c, p, dc, cf, True, at, _): {
			() with eexpr = TField(expr, FStatic(dc, cf));
			etype = at;
			epos = pos
		};
		case FAnonField(cf): {
			() with eexpr = TField(expr, FAnon(cf));
			etype = cf.cf_type;
			epos = pos
		};
		case FDynamicField(t): {
			() with eexpr = TField(expr, FDynamic(field));
			etype = t;
			epos = pos
		};
		case FNotFound: {
			() with eexpr = TField(expr, FDynamic(field));
			etype = t_dynamic;
			epos = pos
		};
		case FEnumField(_): assert False;
		};
	};

	public static function mk_iterator_access(gen, t, expr) return {
		var pos = expr.epos;
		var itf = mk_field_access(gen, expr, "iterator", pos);
		{
			() with eexpr = TCall(itf, []);
			epos = pos;
			etype = snd(get_fun(itf.etype))
		};
	};

	public static var max_dep = 10000.0;

	public static var min_dep = -10000.0;

	public static function solve_deps(name, depslist(t_dependency)) return {
		var vmin = -.(min_dep, 1.0);
		var vmax = +.(max_dep, 1.0);
		function loop(dep, vmin, vmax) return {
			switch (dep) {
			case []: if ( >= (vmin, vmax)) {
					raise(ImpossibleDependency(name));
				} else {
					[];
				};
				/ .(+.(vmin, vmax), 2.0);
			case ::(head, tail): switch (head) {
				case DBefore(f): loop(tail, max(vmin, f), vmax);
				case DAfter(f): loop(tail, vmin, min(vmax, f));
				};
			};
		};
		loop(deps, vmin, vmax);
	};

	public static function get_type(gen, path) return {
		try {
			Hashtbl.find(gen.gtypes, path);
		} catch (e: Not_found) {
			raise(TypeNotFound(path));
		};
	};

	public static var fun_args = List.map(function (v, s): (new Tuple(v.v_name, switch (s) {
case None: False;
case Some(_): True;
	}, v.v_type)))
}
;
