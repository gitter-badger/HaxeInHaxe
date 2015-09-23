import Ast;

typedef Path = Tuple<List<String>, String>;

enum Field_kind {
	Var(value: Var_kind);
	Method(value: Method_kind);
};

typedef Var_kind = {
	v_read : Var_access,
	v_write : Var_access
};

enum Var_access {
	AccNormal;
	AccNo;
	AccNever;
	AccResolve;
	AccCall;
	AccInline;
	AccRequire(value: StringOption<String>);
};

enum Method_kind {
	MethNormal;
	MethInline;
	MethDynamic;
	MethMacro;
};

enum T {
	TMono(value: Ref<T>);
	TEnum(value: TenumTparams);
	TInst(value: TclassTparams);
	TType(value: TdefTparams);
	TFun(value: List<Tuple<String, Bool, T>>T);
	TAnon(value: Tanon);
	TDynamic(value: T);
	TLazy(value: Ref < Unit -> T > );
	TAbstract(value: TabstractTparams);
};

typedef Tparams = List<T>;

typedef Type_params = List<Tuple<String, T>>;

enum Tconstant {
	TInt(value: Int32);
	TFloat(value: String);
	TString(value: String);
	TBool(value: Bool);
	TNull;
	TThis;
	TSuper;
};

typedef Tvar = {
	v_id : Int,
	v_name : String,
	v_type : T,
	v_capture : Bool,
	v_extra : Option<Tuple<Type_params, Option<Texpr>>>,
	v_meta : Metadata
};

typedef Tfunc = {
	tf_args : List<Tuple<Tvar, Option<Tconstant>>>,
	tf_type : T,
	tf_expr : Texpr
};

enum Anon_status {
	Closed;
	Opened;
	Const;
	Extend(value: List<T>);
	Statics(value: Tclass);
	EnumStatics(value: Tenum);
	AbstractStatics(value: Tabstract);
};

typedef Tanon = {
	a_fields : PMap<String, Tclass_field>,
	a_status : Ref<Anon_status>
};

enum Texpr_expr {
	TConst(value: Tconstant);
	TLocal(value: Tvar);
	TArray(value: TexprTexpr);
	TBinop(value: Ast.BinopTexprTexpr);
	TField(value: TexprTfield_access);
	TTypeExpr(value: Module_type);
	TParenthesis(value: Texpr);
	TObjectDecl(value: List<Tuple<String, Texpr>>);
	TArrayDecl(value: List<Texpr>);
	TCall(value: TexprList<Texpr>);
	TNew(value: TclassTparamsList<Texpr>);
	TUnop(value: Ast.UnopAst.Unop_flagTexpr);
	TFunction(value: Tfunc);
	TVar(value: TvarOption<Texpr>);
	TBlock(value: List<Texpr>);
	TFor(value: TvarTexprTexpr);
	TIf(value: TexprTexprOption<Texpr>);
	TWhile(value: TexprTexprAst.While_flag);
	TSwitch(value: TexprList<Tuple<List<Texpr>, Texpr>>Option<Texpr>);
	TTry(value: TexprList<Tuple<Tvar, Texpr>>);
	TReturn(value: Option<Texpr>);
	TBreak;
	TContinue;
	TThrow(value: Texpr);
	TCast(value: TexprOption<Module_type>);
	TMeta(value: Metadata_entryTexpr);
	TEnumParameter(value: TexprTenum_fieldInt);
};

enum Tfield_access {
	FInstance(value: TclassTparamsTclass_field);
	FStatic(value: TclassTclass_field);
	FAnon(value: Tclass_field);
	FDynamic(value: String);
	FClosure(value: Option<Tuple<Tclass, Tparams>>Tclass_field);
	FEnum(value: TenumTenum_field);
};

typedef Texpr = {
	eexpr : Texpr_expr,
	etype : T,
	epos : Ast.Pos
};

typedef Tclass_field = {
	cf_name : String,
	cf_type : T,
	cf_public : Bool,
	cf_pos : Pos,
	cf_doc : Ast.Documentation,
	cf_meta : Metadata,
	cf_kind : Field_kind,
	cf_params : Type_params,
	cf_expr : Option<Texpr>,
	cf_overloads : List<Tclass_field>
};

enum Tclass_kind {
	KNormal;
	KTypeParameter(value: List<T>);
	KExtension(value: TclassTparams);
	KExpr(value: Ast.Expr);
	KGeneric;
	KGenericInstance(value: TclassTparams);
	KMacroType;
	KGenericBuild(value: List<Class_field>);
	KAbstractImpl(value: Tabstract);
};

typedef Metadata = Ast.Metadata;

typedef Tinfos = {
	mt_path : Path,
	mt_module : Module_def,
	mt_pos : Ast.Pos,
	mt_private : Bool,
	mt_doc : Ast.Documentation,
	mt_meta : Metadata,
	mt_params : Type_params
};

typedef Tclass = {
	cl_path : Path,
	cl_module : Module_def,
	cl_pos : Ast.Pos,
	cl_private : Bool,
	cl_doc : Ast.Documentation,
	cl_meta : Metadata,
	cl_params : Type_params,
	cl_kind : Tclass_kind,
	cl_extern : Bool,
	cl_interface : Bool,
	cl_super : Option<Tuple<Tclass, Tparams>>,
	cl_implements : List<Tuple<Tclass, Tparams>>,
	cl_fields : PMap<String, Tclass_field>,
	cl_statics : PMap<String, Tclass_field>,
	cl_ordered_statics : List<Tclass_field>,
	cl_ordered_fields : List<Tclass_field>,
	cl_dynamic : Option<T>,
	cl_array_access : Option<T>,
	cl_constructor : Option<Tclass_field>,
	cl_init : Option<Texpr>,
	cl_overrides : List<Tclass_field>,
	cl_build : Unit -> Bool,
	cl_restore : Unit -> Unit
};

typedef Tenum_field = {
	ef_name : String,
	ef_type : T,
	ef_pos : Ast.Pos,
	ef_doc : Ast.Documentation,
	ef_index : Int,
	ef_params : Type_params,
	ef_meta : Metadata
};

typedef Tenum = {
	e_path : Path,
	e_module : Module_def,
	e_pos : Ast.Pos,
	e_private : Bool,
	e_doc : Ast.Documentation,
	e_meta : Metadata,
	e_params : Type_params,
	e_type : Tdef,
	e_extern : Bool,
	e_constrs : PMap<String, Tenum_field>,
	e_names : List<String>
};

typedef Tdef = {
	t_path : Path,
	t_module : Module_def,
	t_pos : Ast.Pos,
	t_private : Bool,
	t_doc : Ast.Documentation,
	t_meta : Metadata,
	t_params : Type_params,
	t_type : T
};

typedef Tabstract = {
	a_path : Path,
	a_module : Module_def,
	a_pos : Ast.Pos,
	a_private : Bool,
	a_doc : Ast.Documentation,
	a_meta : Metadata,
	a_params : Type_params,
	a_ops : List<Tuple<Ast.Binop, Tclass_field>>,
	a_unops : List<Tuple<Ast.Unop, Unop_flag, Tclass_field>>,
	a_impl : Option<Tclass>,
	a_this : T,
	a_from : List<T>,
	a_from_field : List<Tuple<T, Tclass_field>>,
	a_to : List<T>,
	a_to_field : List<Tuple<T, Tclass_field>>,
	a_array : List<Tclass_field>,
	a_resolve : Option<Tclass_field>
};

enum Module_type {
	TClassDecl(value: Tclass);
	TEnumDecl(value: Tenum);
	TTypeDecl(value: Tdef);
	TAbstractDecl(value: Tabstract);
};

typedef Module_def = {
	m_id : Int,
	m_path : Path,
	m_types : List<Module_type>,
	m_extra : Module_def_extra
};

typedef Module_def_extra = {
	m_file : String,
	m_sign : String,
	m_time : Float,
	m_dirty : Bool,
	m_added : Int,
	m_mark : Int,
	m_deps : PMap<Int, Module_def>,
	m_processed : Int,
	m_kind : Module_kind,
	m_binded_res : PMap<String, String>,
	m_macro_calls : List<String>,
	m_if_feature : List<Tuple<String, Tuple<Tclass, Tclass_field, Bool>>>,
	m_features : Hashtbl<String, Bool>
};

enum Module_kind {
	MCode;
	MMacro;
	MFake;
	MSub;
	MExtern;
};

enum Dt {
	DTSwitch(value: TexprList<Tuple<Texpr, Dt>>Option<Dt>);
	DTBind(value: List<Tuple<Tuple<Tvar, Pos>, Texpr>>Dt);
	DTGoto(value: Int);
	DTExpr(value: Texpr);
	DTGuard(value: TexprDtOption<Dt>);
};

typedef Decision_tree = {
	dt_dt_lookup : Array<Dt>,
	dt_first : Int,
	dt_type : T,
	dt_var_init : List<Tuple<Tvar, Option<Texpr>>>,
	dt_is_complex : Bool
};

enum Unify_error {
	Cannot_unify(value: TT);
	Invalid_field_type(value: String);
	Has_no_field(value: TString);
	Has_no_runtime_field(value: TString);
	Has_extra_field(value: TString);
	Invalid_kind(value: StringField_kindField_kind);
	Invalid_visibility(value: String);
	Not_matching_optional(value: String);
	Cant_force_optional;
	Invariant_parameter(value: TT);
	Constraint_failure(value: String);
	Missing_overload(value: Tclass_fieldT);
	Unify_custom(value: String);
};

class /*exception*/ Unify_error {

};

enum Eq_kind {
	EqStrict;
	EqCoreType;
	EqRightDynamic;
	EqBothDynamic;
	EqDoNotFollowNull;
};

class Type {
	public static var alloc_var = var uid = ref(0);
	function n: function t: incr(uid);
	{
		() with v_name = n;
		v_type = t;
		v_id = uid.val;
		v_capture = False;
		v_extra = None;
		v_meta = []
	};

	public static function alloc_unbound_var(n, t) return {
		var v = alloc_var(n, t);
		v.v_meta = ::((new Tuple(Meta.Unbound, [], null_pos)), []);
		v;
	};

	public static var alloc_mid = var mid = ref(0);
	function []: incr(mid);
	mid.val;

	public static function mk(e, t, p) return {
		{
			() with eexpr = e;
			etype = t;
			epos = p
		};
	};

	public static function mk_block(e) return {
		switch (e.eexpr) {
		case TBlock(_): e;
		case _: mk(TBlock(::(e, [])), e.etype, e.epos);
		};
	};

	public static function mk_cast(e, t, p) return {
		mk(TCast(e, None), t, p);
	};

	public static function null(t, p) return {
		mk(TConst(TNull), t, p);
	};

	public static function mk_mono([]) return {
		TMono(ref(None));
	};

	public static var t_dynamic = TDynamic(t_dynamic);

	public static function tfun(pl, r) return {
		TFun(List.map(function t: (new Tuple("", False, t)), pl), r);
	};

	public static function fun_args(l) return {
		List.map(function (a, c, t): (new Tuple(a, <>(c, None), t)), l);
	};

	public static function mk_class(m, path, pos) return {
		{
			() with cl_path = path;
			cl_module = m;
			cl_pos = pos;
			cl_doc = None;
			cl_meta = [];
			cl_private = False;
			cl_kind = KNormal;
			cl_extern = False;
			cl_interface = False;
			cl_params = [];
			cl_super = None;
			cl_implements = [];
			cl_fields = PMap.empty;
			cl_ordered_statics = [];
			cl_ordered_fields = [];
			cl_statics = PMap.empty;
			cl_dynamic = None;
			cl_array_access = None;
			cl_constructor = None;
			cl_init = None;
			cl_overrides = [];
			cl_build = function []: True;
			cl_restore = function []: []
		};
	};

	public static function module_extra(file, sign, time, kind) return {
		{
			() with m_file = file;
			m_sign = sign;
			m_dirty = False;
			m_added = 0;
			m_mark = 0;
			m_time = time;
			m_processed = 0;
			m_deps = PMap.empty;
			m_kind = kind;
			m_binded_res = PMap.empty;
			m_macro_calls = [];
			m_if_feature = [];
			m_features = Hashtbl.create(0)
		};
	};

	public static function mk_field(name, t, p) return {
		{
			() with cf_name = name;
			cf_type = t;
			cf_pos = p;
			cf_doc = None;
			cf_meta = [];
			cf_public = True;
			cf_kind = Var({
				() with v_read = AccNormal;
				v_write = AccNormal
			});
			cf_expr = None;
			cf_params = [];
			cf_overloads = []
		};
	};

	public static var null_module = { () with m_id = alloc_mid([]);
									  m_path = (new Tuple([], ""));
									  m_types = [];
									  m_extra = module_extra("", "", 0., MFake)
									};

	public static var null_class = var c = mk_class(null_module, (new Tuple([], "")), Ast.null_pos);
	c.cl_private = True;
	c;

	public static var null_field = mk_field("", t_dynamic, Ast.null_pos);

	public static var null_abstract = { () with a_path = (new Tuple([], ""));
										a_module = null_module;
										a_pos = null_pos;
										a_private = True;
										a_doc = None;
										a_meta = [];
										a_params = [];
										a_ops = [];
										a_unops = [];
										a_impl = None;
										a_this = t_dynamic;
										a_from = [];
										a_from_field = [];
										a_to = [];
										a_to_field = [];
										a_array = [];
										a_resolve = None
									  };

	public static function add_dependency(m, mdep) return {
		if ( && ( != (m, null_module), != (m, mdep))) {
			m.m_extra.m_deps = PMap.add(mdep.m_id, mdep, m.m_extra.m_deps);
		} else {
			[];
		};
	};

	public static function arg_name(Tuple(a, _)) return {
		a.v_name;
	};

	public static function t_infos(t) return {
		switch (t) {
		case TClassDecl(c): Obj.magic(c);
		case TEnumDecl(e): Obj.magic(e);
		case TTypeDecl(t): Obj.magic(t);
		case TAbstractDecl(a): Obj.magic(a);
		} : tinfos;
	};

	public static function t_path(t) return {
		t_infos(t).mt_path;
	};

	public static function is_parent(csup, c) return {
		if ( || ( == (c, csup), List.exists(function (i, _): is_parent(csup, i), c.cl_implements))) {
			True;
		} else {
			switch (c.cl_super) {
			case None: False;
			case Some(c, _): is_parent(csup, c);
			};
		};
	};

	public static function map(loop, t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: t;
			case Some(t): loop(t);
			};
		case TEnum(_, []) | TInst(_, []) | TType(_, []): t;
		case TEnum(e, tl): TEnum(e, List.map(loop, tl));
		case TInst(c, tl): TInst(c, List.map(loop, tl));
		case TType(t2, tl): TType(t2, List.map(loop, tl));
		case TAbstract(a, tl): TAbstract(a, List.map(loop, tl));
		case TFun(tl, r): TFun(List.map(function (s, o, t): (new Tuple(s, o, loop(t))), tl), loop(r));
		case TAnon(a): var fields = PMap.map(function f: {
													 (f) with cf_type = loop(f.cf_type)
												 }, a.a_fields);
			switch (a.a_status.val) {
			case Opened: a.a_fields = fields;
				t;
			case _: TAnon({ () with a_fields = fields;
								a_status = a.a_status
							  });
			};
		case TLazy(f): var ft = f.val([]);
			var ft2 = loop(ft);
			if ( == (ft, ft2)) {
				t;
			} else {
				ft2;
			};
		case TDynamic(t2): if ( == (t, t2)) {
				t;
			} else {
				TDynamic(loop(t2));
			};
		};
	};

	public static function apply_params(cparams, params, t) return {
		switch (cparams) {
		case []: t;
		case _: 	function loop(l1, l2) return {
				switch ((new Tuple(l1, l2))) {
				case ([], []): [];
				case (::((x, TLazy(f)), l1), _): loop(::((new Tuple(x, f.val([]))), l1), l2);
				case (::((_, t1), l1), ::(t2, l2)): ::((new Tuple(t1, t2)), loop(l1, l2));
				case _: assert False;
				};
			};
			var subst = loop(cparams, params);
			function loop(t) return {
				try {
					List.assq(t, subst);
				} catch (e: Not_found) {
					switch (t) {
					case TMono(r): switch (r.val) {
						case None: t;
						case Some(t): loop(t);
						};
					case TEnum(e, tl): switch (tl) {
						case []: t;
						case _: TEnum(e, List.map(loop, tl));
						};
					case TType(t2, tl): switch (tl) {
						case []: t;
						case _: TType(t2, List.map(loop, tl));
						};
					case TAbstract(a, tl): switch (tl) {
						case []: t;
						case _: TAbstract(a, List.map(loop, tl));
						};
					case TInst(c, tl): switch (tl) {
						case []: t;
						case ::(TMono(r), []): switch (r.val) {
							case Some(tt) if (==(t, tt)): var pt = mk_mono([]);
								var t = TInst(c, ::(pt, []));
								switch (pt) {
								case TMono(r): r.val = Some(t);
								case _: assert False;
								};
								t;
							case _: TInst(c, List.map(loop, tl));
							};
						case _: TInst(c, List.map(loop, tl));
						};
					case TFun(tl, r): TFun(List.map(function (s, o, t): (new Tuple(s, o, loop(t))), tl), loop(r));
					case TAnon(a): var fields = PMap.map(function f: {
																 (f) with cf_type = loop(f.cf_type)
															 }, a.a_fields);
						switch (a.a_status.val) {
						case Opened: a.a_fields = fields;
							t;
						case _: TAnon({ () with a_fields = fields;
											a_status = a.a_status
										  });
						};
					case TLazy(f): var ft = f.val([]);
						var ft2 = loop(ft);
						if ( == (ft, ft2)) {
							t;
						} else {
							ft2;
						};
					case TDynamic(t2): if ( == (t, t2)) {
							t;
						} else {
							TDynamic(loop(t2));
						};
					};
				};
			};
			loop(t);
		};
	};

	public static function monomorphs(eparams, t) return {
		apply_params(eparams, List.map(function _: mk_mono([]), eparams), t);
	};

	public static function follow(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case Some(t): follow(t);
			case _: t;
			};
		case TLazy(f): follow(f.val([]));
		case TType(t, tl): follow(apply_params(t.t_params, tl, t.t_type));
		case _: t;
		};
	};

	public static function is_nullable(match) return switch (match) {
	case TMono(r): switch (r.val) {
		case None: False;
		case Some(t): is_nullable(t);
		};
	case TType({ t_path = ([], Null) }, ::(_, [])): True;
	case TLazy(f): is_nullable(f.val([]));
	case TType(t, tl): is_nullable(apply_params(t.t_params, tl, t.t_type));
	case TFun(_): False;
	case TAbstract(a, _) if (Meta.has(Meta.CoreType, a.a_meta)): !(Meta.has(Meta.NotNull, a.a_meta));
	case TAbstract(a, tl): && (!(Meta.has(Meta.NotNull, a.a_meta)), is_nullable(apply_params(a.a_params, tl, a.a_this)));
	case _: True;
	};

	public static function is_null( ? : (no_lazy = False)) return {
	case TMono(r): switch (r.val) {
		case None: False;
		case Some(t): is_null(t);
		};
	case TType({ t_path = ([], Null) }, ::(t, [])): !(is_nullable(follow(t)));
	case TLazy(f): if (no_lazy) {
			raise(Exit);
		} else {
			is_null(f.val([]));
		};
	case TType(t, tl): is_null(apply_params(t.t_params, tl, t.t_type));
	case _: False;
	};

	public static function is_explicit_null(match) return switch (match) {
	case TMono(r): switch (r.val) {
		case None: False;
		case Some(t): is_null(t);
		};
	case TType({ t_path = ([], Null) }, ::(t, [])): True;
	case TLazy(f): is_null(f.val([]));
	case TType(t, tl): is_null(apply_params(t.t_params, tl, t.t_type));
	case _: False;
	};

	public static function has_mono(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: True;
			case Some(t): has_mono(t);
			};
		case TInst(_, pl) | TEnum(_, pl) | TAbstract(_, pl) | TType(_, pl): List.exists(has_mono, pl);
		case TDynamic(_): False;
		case TFun(args, r): || (has_mono(r), List.exists(function (_, _, t): has_mono(t), args));
		case TAnon(a): PMap.fold(function cf: function b: || (has_mono(cf.cf_type), b), a.a_fields, False);
		case TLazy(r): has_mono(r.val([]));
		};
	};

	public static function concat(e1, e2) return {
		var e = switch ((new Tuple(e1.eexpr, e2.eexpr))) {
		case (TBlock(el1), TBlock(el2)): TBlock(@(el1, el2));
		case (TBlock(el), _): TBlock(@(el, ::(e2, [])));
		case (_, TBlock(el)): TBlock(::(e1, el));
		case (_, _): TBlock(::(e1, ::(e2, [])));
		};
		mk(e, e2.etype, punion(e1.epos, e2.epos));
	};

	public static function is_closed(a) return {
		<>(a.a_status.val, Opened);
	};

	public static function type_of_module_type(match) return switch (match) {
	case TClassDecl(c): TInst(c, List.map(snd, c.cl_params));
	case TEnumDecl(e): TEnum(e, List.map(snd, e.e_params));
	case TTypeDecl(t): TType(t, List.map(snd, t.t_params));
	case TAbstractDecl(a): TAbstract(a, List.map(snd, a.a_params));
	};

	public static function tconst_to_const(match) return switch (match) {
	case TInt(i): Int(Int32.to_string(i));
	case TFloat(s): Float(s);
	case TString(s): String(s);
	case TBool(b): Ident(if (b) {
		"true";
	} else {
		"false";
	});
	case TNull: Ident("null");
	case TThis: Ident("this");
	case TSuper: Ident("super");
	};

	public static function field_name(f) return {
		switch (f) {
		case FAnon(f) | FInstance(_, _, f) | FStatic(_, f) | FClosure(_, f): f.cf_name;
		case FEnum(_, f): f.ef_name;
		case FDynamic(n): n;
		};
	};

	public static function extract_field(match) return switch (match) {
	case FAnon(f) | FInstance(_, _, f) | FStatic(_, f) | FClosure(_, f): Some(f);
	case _: None;
	};

	public static function is_extern_field(f) return {
		switch (f.cf_kind) {
		case Method(_): False;
		case Var({ v_read = AccNormal | AccInline | AccNo }) | Var({ v_write = AccNormal | AccNo }): False;
		case _: !(Meta.has(Meta.IsVar, f.cf_meta));
		};
	};

	public static function field_type(f) return {
		switch (f.cf_params) {
		case []: f.cf_type;
		case l: monomorphs(l, f.cf_type);
		};
	};

	public static function raw_class_field(build_type, c, tl, i) return {
		var apply = apply_params(c.cl_params, tl);
		try {
			var f = PMap.find(i, c.cl_fields);
			(new Tuple(Some(c, tl), build_type(f), f));
		} catch (e: Not_found) {
			try {
				switch (c.cl_constructor) {
				case Some(ctor) if (=(i, "new")): (new Tuple(Some(c, tl), build_type(ctor), ctor));
				case _: raise(Not_found);
				};
			} catch (e: Not_found) {
				try {
					switch (c.cl_super) {
					case None: raise(Not_found);
					case Some(c, tl): var Tuple(c2, t, f) = raw_class_field(build_type, c, List.map(apply, tl), i);
						(new Tuple(c2, apply_params(c.cl_params, tl, t), f));
					};
				} catch (e: Not_found) {
					switch (c.cl_kind) {
					case KTypeParameter(tl): function loop(match) return switch (match) {
						case []: raise(Not_found);
						case ::(t, ctl): switch (follow(t)) {
							case TAnon(a): try {
									var f = PMap.find(i, a.a_fields);
									(new Tuple(None, build_type(f), f));
								} catch (e: Not_found) {
									loop(ctl);
								};
							case TInst(c, tl): try {
									var Tuple(c2, t, f) = raw_class_field(build_type, c, List.map(apply, tl), i);
									(new Tuple(c2, apply_params(c.cl_params, tl, t), f));
								} catch (e: Not_found) {
									loop(ctl);
								};
							case _: loop(ctl);
							};
						};
						loop(tl);
					case _: if (!(c.cl_interface)) {
							raise(Not_found);
						} else {
							[];
						};
						function loop(match) return switch (match) {
						case []: raise(Not_found);
						case ::((c, tl), l): try {
								var Tuple(c2, t, f) = raw_class_field(build_type, c, List.map(apply, tl), i);
								(new Tuple(c2, apply_params(c.cl_params, tl, t), f));
							} catch (e: Not_found) {
								loop(l);
							};
						};
						loop(c.cl_implements);
					};
				};
			};
		};
	};

	public static var class_field = raw_class_field(field_type);

	public static function quick_field(t, n) return {
		switch (follow(t)) {
		case TInst(c, tl): var Tuple(c, _, f) = raw_class_field(function f: f.cf_type, c, tl, n);
			switch (c) {
			case None: FAnon(f);
			case Some(c, tl): FInstance(c, tl, f);
			};
		case TAnon(a): switch (a.a_status.val) {
			case EnumStatics(e): var ef = PMap.find(n, e.e_constrs);
				FEnum(e, ef);
			case Statics(c): FStatic(c, PMap.find(n, c.cl_statics));
			case AbstractStatics(a): switch (a.a_impl) {
				case Some(c): var cf = PMap.find(n, c.cl_statics);
					FStatic(c, cf);
				case _: raise(Not_found);
				};
			case _: FAnon(PMap.find(n, a.a_fields));
			};
		case TDynamic(_): FDynamic(n);
		case TEnum(_) | TMono(_) | TAbstract(_) | TFun(_): raise(Not_found);
		case TLazy(_) | TType(_): assert False;
		};
	};

	public static function quick_field_dynamic(t, s) return {
		try {
			quick_field(t, s);
		} catch (e: Not_found) {
			FDynamic(s);
		};
	};

	public static function get_constructor(build_type, c) return {
		switch ((new Tuple(c.cl_constructor, c.cl_super))) {
		case (Some(c), _): (new Tuple(build_type(c), c));
		case (None, None): raise(Not_found);
		case (None, Some(csup, cparams)): var Tuple(t, c) = get_constructor(build_type, csup);
			(new Tuple(apply_params(csup.cl_params, cparams, t), c));
		};
	};

	public static function print_context([]) return {
		ref([]);
	};

	public static function s_type_kind(t) return {
		function map(tl) return {
			String.concat(", ", List.map(s_type_kind, tl));
		};
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: "TMono [None]";
			case Some(t): ^ ("TMono [Some [", ^ (s_type_kind(t), "]]"));
			};
		case TEnum(en, tl): Printf.sprintf("TEnum[%s, [%s]]", s_type_path(en.e_path), map(tl));
		case TInst(c, tl): Printf.sprintf("TInst[%s, [%s]]", s_type_path(c.cl_path), map(tl));
		case TType(t, tl): Printf.sprintf("TType[%s, [%s]]", s_type_path(t.t_path), map(tl));
		case TAbstract(a, tl): Printf.sprintf("TAbstract[%s, [%s]]", s_type_path(a.a_path), map(tl));
		case TFun(tl, r): Printf.sprintf("TFun[[%s], %s]", String.concat(", ", List.map(function (n, b,
			t): Printf.sprintf("%s%s:%s", if (b) {
			"?";
		} else {
			"";
		}, n, s_type_kind(t)), tl)), s_type_kind(r));
		case TAnon(an): "TAnon";
		case TDynamic(t2): "TDynamic";
		case TLazy(_): "TLazy";
		};
	};

	public static function s_type(ctx, t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: Printf.sprintf("Unknown<%d>", try {
					List.assq(t, ctx.val);
				} catch (e: Not_found) {
					var n = List.length(ctx.val);
					ctx.val = ::((new Tuple(t, n)), ctx.val);
					n;
				});
			case Some(t): s_type(ctx, t);
			};
		case TEnum(e, tl): ^ (Ast.s_type_path(e.e_path), s_type_params(ctx, tl));
		case TInst(c, tl): switch (c.cl_kind) {
			case KExpr(e): Ast.s_expr(e);
			case _: ^ (Ast.s_type_path(c.cl_path), s_type_params(ctx, tl));
			};
		case TType(t, tl): ^ (Ast.s_type_path(t.t_path), s_type_params(ctx, tl));
		case TAbstract(a, tl): ^ (Ast.s_type_path(a.a_path), s_type_params(ctx, tl));
		case TFun([], t): ^ ("Void -> ", s_fun(ctx, t, False));
		case TFun(l, t): ^ (String.concat(" -> ", List.map(function (s, b, t): ^ (if (b) {
			"?";
		} else {
			"";
		}, ^ (if ( = (s, "")) {
			"";
		} else {
			^ (s, " : ");
			}, s_fun(ctx, t, True))), l)), ^ (" -> ", s_fun(ctx, t, False)));
		case TAnon(a): var fl = PMap.fold(function f: function acc: ::( ^ (if (Meta.has(Meta.Optional, f.cf_meta)) {
			" ?";
		} else {
			" ";
		}, ^ (f.cf_name, ^ (" : ", s_type(ctx, f.cf_type)))), acc), a.a_fields, []);
			^ ("{", ^ (if (!(is_closed(a))) {
			"+";
		} else {
			"";
		}, ^ (String.concat(",", fl), " }")));
		case TDynamic(t2): ^ ("Dynamic", s_type_params(ctx, if ( == (t, t2)) {
			[];
			} else {
				::(t2, []);
			}));
		case TLazy(f): s_type(ctx, f.val([]));
		};
	};

	public static function s_fun(ctx, t, void) return {
		switch (t) {
		case TFun(_): ^ ("[", ^ (s_type(ctx, t), "]"));
		case TAbstract({ a_path = ([], Void) }, []) if (void): ^ ("[", ^ (s_type(ctx, t), "]"));
		case TMono(r): switch (r.val) {
			case None: s_type(ctx, t);
			case Some(t): s_fun(ctx, t, void);
			};
		case TLazy(f): s_fun(ctx, f.val([]), void);
		case _: s_type(ctx, t);
		};
	};

	public static function s_type_params(ctx) return {
	case []: "";
	case l: ^ ("<", ^ (String.concat(", ", List.map(s_type(ctx), l)), ">"));
	};

	public static function s_access(is_read) return {
	case AccNormal: "default";
	case AccNo: "null";
	case AccNever: "never";
	case AccResolve: "resolve";
	case AccCall: if (is_read) {
			"get";
		} else {
			"set";
		};
	case AccInline: "inline";
	case AccRequire(n, _): ^ ("require ", n);
	};

	public static function s_kind(match) return switch (match) {
	case Var({ v_read = AccNormal; v_write = AccNormal }): "var";
	case Var(v): ^ ("[", ^ (s_access(True, v.v_read), ^ (",", ^ (s_access(False, v.v_write), "]"))));
	case Method(m): switch (m) {
		case MethNormal: "method";
		case MethDynamic: "dynamic method";
		case MethInline: "inline method";
		case MethMacro: "macro method";
		};
	};

	public static function s_expr_kind(e) return {
		switch (e.eexpr) {
		case TConst(_): "Const";
		case TLocal(_): "Local";
		case TArray(_, _): "Array";
		case TBinop(_, _, _): "Binop";
		case TEnumParameter(_, _, _): "EnumParameter";
		case TField(_, _): "Field";
		case TTypeExpr(_): "TypeExpr";
		case TParenthesis(_): "Parenthesis";
		case TObjectDecl(_): "ObjectDecl";
		case TArrayDecl(_): "ArrayDecl";
		case TCall(_, _): "Call";
		case TNew(_, _, _): "New";
		case TUnop(_, _, _): "Unop";
		case TFunction(_): "Function";
		case TVar(_): "Vars";
		case TBlock(_): "Block";
		case TFor(_, _, _): "For";
		case TIf(_, _, _): "If";
		case TWhile(_, _, _): "While";
		case TSwitch(_, _, _): "Switch";
		case TTry(_, _): "Try";
		case TReturn(_): "Return";
		case TBreak: "Break";
		case TContinue: "Continue";
		case TThrow(_): "Throw";
		case TCast(_): "Cast";
		case TMeta(_): "Meta";
		};
	};

	public static function s_const(match) return switch (match) {
	case TInt(i): Int32.to_string(i);
	case TFloat(s): s;
	case TString(s): Printf.sprintf("\"%s\"", Ast.s_escape(s));
	case TBool(b): if (b) {
			"true";
		} else {
			"false";
		};
	case TNull: "null";
	case TThis: "this";
	case TSuper: "super";
	};

	public static function s_expr(s_type, e) return {
		var sprintf = Printf.sprintf;
		function slist(f, l) return {
			String.concat(",", List.map(f, l));
		};
		var loop = s_expr(s_type);
		function s_var(v) return {
			^ (v.v_name, ^ (":", ^ (string_of_int(v.v_id), if (v.v_capture) {
			"[c]";
		} else {
			"";
		})));
		};
		var str = switch (e.eexpr) {
		case TConst(c): ^ ("Const ", s_const(c));
		case TLocal(v): ^ ("Local ", s_var(v));
		case TArray(e1, e2): sprintf("%s[%s]", loop(e1), loop(e2));
		case TBinop(op, e1, e2): sprintf("[%s %s %s]", loop(e1), s_binop(op), loop(e2));
		case TEnumParameter(e1, _, i): sprintf("%s[%i]", loop(e1), i);
		case TField(e, f): var fstr = switch (f) {
			case FStatic(c, f): ^ ("static[", ^ (s_type_path(c.cl_path), ^ (".", ^ (f.cf_name, "]"))));
			case FInstance(c, _, f): ^ ("inst[", ^ (s_type_path(c.cl_path), ^ (".", ^ (f.cf_name, ^ (" : ", ^ (s_type(f.cf_type),
														"]"))))));
			case FClosure(c, f): ^ ("closure[", ^ (switch (c) {
			case None: f.cf_name;
			case Some(c, _): ^ (s_type_path(c.cl_path), ^ (".", f.cf_name));
				}, "]"));
			case FAnon(f): ^ ("anon[", ^ (f.cf_name, "]"));
			case FEnum(en, f): ^ ("enum[", ^ (s_type_path(en.e_path), ^ (".", ^ (f.ef_name, "]"))));
			case FDynamic(f): ^ ("dynamic[", ^ (f, "]"));
			};
			sprintf("%s.%s", loop(e), fstr);
		case TTypeExpr(m): sprintf("TypeExpr %s", s_type_path(t_path(m)));
		case TParenthesis(e): sprintf("Parenthesis %s", loop(e));
		case TObjectDecl(fl): sprintf("ObjectDecl {%s}", slist(function (f, e): sprintf("%s : %s", f, loop(e)), fl));
		case TArrayDecl(el): sprintf("ArrayDecl [%s]", slist(loop, el));
		case TCall(e, el): sprintf("Call %s[%s]", loop(e), slist(loop, el));
		case TNew(c, pl, el): sprintf("New %s%s[%s]", s_type_path(c.cl_path), switch (pl) {
		case []: "";
			case l: sprintf("<%s>", slist(s_type, l));
			}, slist(loop, el));
		case TUnop(op, f, e): switch (f) {
			case Prefix: sprintf("[%s %s]", s_unop(op), loop(e));
			case Postfix: sprintf("[%s %s]", loop(e), s_unop(op));
			};
		case TFunction(f): var args = slist(function (v, o): sprintf("%s : %s%s", s_var(v), s_type(v.v_type), switch (o) {
		case None: "";
		case Some(c): ^ (" = ", s_const(c));
			}), f.tf_args);
			sprintf("Function[%s] : %s = %s", args, s_type(f.tf_type), loop(f.tf_expr));
		case TVar(v, eo): sprintf("Vars %s", sprintf("%s : %s%s", s_var(v), s_type(v.v_type), switch (eo) {
		case None: "";
		case Some(e): ^ (" = ", loop(e));
			}));
		case TBlock(el): sprintf("Block {\n%s}", String.concat("", List.map(function e: sprintf("%s;\n", loop(e)), el)));
		case TFor(v, econd, e): sprintf("For [%s : %s in %s,%s]", s_var(v), s_type(v.v_type), loop(econd), loop(e));
		case TIf(e, e1, e2): sprintf("If [%s,%s%s]", loop(e), loop(e1), switch (e2) {
		case None: "";
		case Some(e): ^ (",", loop(e));
			});
		case TWhile(econd, e, flag): switch (flag) {
			case NormalWhile: sprintf("While [%s,%s]", loop(econd), loop(e));
			case DoWhile: sprintf("DoWhile [%s,%s]", loop(e), loop(econd));
			};
		case TSwitch(e, cases, def): sprintf("Switch [%s,[%s]%s]", loop(e), slist(function (cl, e): sprintf("case %s: %s",
			slist(loop, cl), loop(e)), cases), switch (def) {
		case None: "";
		case Some(e): ^ (",", loop(e));
			});
		case TTry(e, cl): sprintf("Try %s[%s] ", loop(e), slist(function (v, e): sprintf("catch[ %s : %s ] %s", s_var(v),
									  s_type(v.v_type), loop(e)), cl));
		case TReturn(None): "Return";
		case TReturn(Some(e)): sprintf("Return %s", loop(e));
		case TBreak: "Break";
		case TContinue: "Continue";
		case TThrow(e): ^ ("Throw ", loop(e));
		case TCast(e, t): sprintf("Cast %s%s", switch (t) {
		case None: "";
		case Some(t): ^ (s_type_path(t_path(t)), ": ");
			}, loop(e));
		case TMeta((n, el, _), e): sprintf("@%s%s %s", Meta.to_string(n), switch (el) {
		case []: "";
			case _: ^ ("[", ^ (String.concat(", ", List.map(Ast.s_expr, el)), "]"));
			}, loop(e));
		};
		sprintf("[%s : %s]", str, s_type(e.etype));
	};

	public static function s_dt(tabs, tree) return {
		var s_type = s_type(print_context([]));
		^ (tabs, switch (tree) {
	case DTSwitch(st, cl, dto): ^ ("switch[", ^ (s_expr(s_type, st), ^ ("] { \n", ^ (tabs, ^ (String.concat( ^ ("\n", tabs),
			List.map(function (c, dt): ^ ("case ", ^ (s_expr(s_type, c), ^ (":\n", s_dt( ^ (tabs, "\t"), dt)))), cl)),
			^ (switch (dto) {
		case None: "";
		case Some(dt): ^ (tabs, ^ ("default: ", s_dt( ^ (tabs, "\t"), dt)));
			}, ^ ("\n", ^ (if ( = (String.length(tabs), 0)) {
			"";
		} else {
			String.sub(tabs, 0, -(String.length(tabs), 1));
			}, "}"))))))));
		case DTBind(bl, dt): ^ ("bind ", ^ (String.concat(",", List.map(function ((v, _), st): ^ (v.v_name, ^ ("[",
												^ (string_of_int(v.v_id), ^ ("] =", s_expr(s_type, st))))), bl)), ^ ("\n", s_dt(tabs, dt))));
		case DTGoto(i): ^ ("goto ", string_of_int(i));
		case DTExpr(e): s_expr(s_type, e);
		case DTGuard(e, dt1, dt2): ^ ("if[", ^ (s_expr(s_type, e), ^ ("] ", ^ (s_dt(tabs, dt1), switch (dt2) {
		case None: "";
		case Some(dt): ^ (" else ", s_dt(tabs, dt));
			}))));
		});
	};

	public static function s_expr_pretty(tabs, s_type, e) return {
		var sprintf = Printf.sprintf;
		var loop = s_expr_pretty(tabs, s_type);
		function slist(f, l) return {
			String.concat(",", List.map(f, l));
		};
		switch (e.eexpr) {
		case TConst(c): s_const(c);
		case TLocal(v): v.v_name;
		case TArray(e1, e2): sprintf("%s[%s]", loop(e1), loop(e2));
		case TBinop(op, e1, e2): sprintf("%s %s %s", loop(e1), s_binop(op), loop(e2));
		case TEnumParameter(e1, _, i): sprintf("%s[%i]", loop(e1), i);
		case TField(e1, s): sprintf("%s.%s", loop(e1), field_name(s));
		case TTypeExpr(mt): s_type_path(t_path(mt));
		case TParenthesis(e1): sprintf("[%s]", loop(e1));
		case TObjectDecl(fl): sprintf("{%s}", slist(function (f, e): sprintf("%s : %s", f, loop(e)), fl));
		case TArrayDecl(el): sprintf("[%s]", slist(loop, el));
		case TCall(e1, el): sprintf("%s[%s]", loop(e1), slist(loop, el));
		case TNew(c, pl, el): sprintf("new %s[%s]", s_type_path(c.cl_path), slist(loop, el));
		case TUnop(op, f, e): switch (f) {
			case Prefix: sprintf("%s %s", s_unop(op), loop(e));
			case Postfix: sprintf("%s %s", loop(e), s_unop(op));
			};
		case TFunction(f): var args = slist(function (v, o): sprintf("%s:%s%s", v.v_name, s_type(v.v_type), switch (o) {
		case None: "";
		case Some(c): ^ (" = ", s_const(c));
			}), f.tf_args);
			sprintf("function[%s] = %s", args, loop(f.tf_expr));
		case TVar(v, eo): sprintf("var %s", sprintf("%s%s", v.v_name, switch (eo) {
		case None: "";
		case Some(e): ^ (" = ", loop(e));
			}));
		case TBlock(el): var ntabs = ^ (tabs, "\t");
			var s = sprintf("{\n%s", String.concat("", List.map(function e: sprintf("%s%s;\n", ntabs, s_expr_pretty(ntabs, s_type, e)),
												   el)));
			^ (s, ^ (tabs, "}"));
		case TFor(v, econd, e): sprintf("for [%s in %s] %s", v.v_name, loop(econd), loop(e));
		case TIf(e, e1, e2): sprintf("if [%s]%s%s", loop(e), loop(e1), switch (e2) {
		case None: "";
		case Some(e): ^ (" else ", loop(e));
			});
		case TWhile(econd, e, flag): switch (flag) {
			case NormalWhile: sprintf("while [%s] %s", loop(econd), loop(e));
			case DoWhile: sprintf("do [%s] while[%s]", loop(e), loop(econd));
			};
		case TSwitch(e, cases, def): var ntabs = ^ (tabs, "\t");
			var s = sprintf("switch [%s] {\n%s%s", loop(e), slist(function (cl, e): sprintf("%scase %s: %s\n", ntabs, slist(loop, cl),
			s_expr_pretty(ntabs, s_type, e)), cases), switch (def) {
		case None: "";
		case Some(e): ^ (ntabs, ^ ("default: ", ^ (s_expr_pretty(ntabs, s_type, e), "\n")));
			});
			^ (s, ^ (tabs, "}"));
		case TTry(e, cl): sprintf("try %s%s", loop(e), slist(function (v, e): sprintf("catch[ %s : %s ] %s", v.v_name,
									  s_type(v.v_type), loop(e)), cl));
		case TReturn(None): "return";
		case TReturn(Some(e)): sprintf("return %s", loop(e));
		case TBreak: "break";
		case TContinue: "continue";
		case TThrow(e): ^ ("throw ", loop(e));
		case TCast(e, None): sprintf("cast %s", loop(e));
		case TCast(e, Some(mt)): sprintf("cast [%s,%s]", loop(e), s_type_path(t_path(mt)));
		case TMeta((n, el, _), e): sprintf("@%s%s %s", Meta.to_string(n), switch (el) {
		case []: "";
			case _: ^ ("[", ^ (String.concat(", ", List.map(Ast.s_expr, el)), "]"));
			}, loop(e));
		};
	};

	public static function s_expr_ast(print_var_ids, tabs, s_type, e) return {
		var sprintf = Printf.sprintf;
		function loop( ? : (extra_tabs = "")) return {
			s_expr_ast(print_var_ids, ^ (tabs, ^ ("\t", extra_tabs)), s_type);
		};
		function tag_args(tabs, sl) return {
			switch (sl) {
			case []: "";
			case ::(s, []) if (!(String.contains(s, '\n'))): ^ (" ", s);
			case _: var tabs = ^ ("\n", ^ (tabs, "\t"));
				^ (tabs, String.concat(tabs, sl));
			};
		};
		function tag(s, ? : (t = None), ? : (extra_tabs = ""), sl) return {
			var st = switch (t) {
			case None: s_type(e.etype);
			case Some(t): s_type(t);
			};
			sprintf("[%s:%s]%s", s, st, tag_args( ^ (tabs, extra_tabs), sl));
		};
		function var_id(v) return {
			if (print_var_ids) {
				v.v_id;
			} else {
				0;
			};
		};
		function const(c) return {
			sprintf("[Const %s:%s]", s_const(c), s_type(e.etype));
		};
		function local(v) return {
			sprintf("[Local %s[%i]:%s]", v.v_name, var_id(v), s_type(v.v_type));
		};
		function var(v, sl) return {
			sprintf("[Var %s[%i]:%s]%s", v.v_name, var_id(v), s_type(v.v_type), tag_args(tabs, sl));
		};
		function module_type(mt) return {
			sprintf("[TypeExpr %s:%s]", s_type_path(t_path(mt)), s_type(e.etype));
		};
		switch (e.eexpr) {
		case TConst(c): const(c);
		case TLocal(v): local(v);
		case TArray(e1, e2): tag("Array", ::(loop(e1), ::(loop(e2), [])));
		case TBinop(op, e1, e2): tag("Binop", ::(loop(e1), ::(s_binop(op), ::(loop(e2), []))));
		case TUnop(op, flag, e1): tag("Unop", ::(s_unop(op), ::(if ( = (flag, Postfix)) {
			"Postfix";
		} else {
			"Prefix";
		}, ::(loop(e1), []))));
		case TEnumParameter(e1, ef, i): tag("EnumParameter", ::(loop(e1), ::(ef.ef_name, ::(string_of_int(i), []))));
		case TField(e1, fa): var sfa = switch (fa) {
			case FInstance(c, tl, cf): tag("FInstance", extra_tabs = "\t", ::(s_type(TInst(c, tl)), ::(cf.cf_name, [])));
			case FStatic(c, cf): tag("FStatic", extra_tabs = "\t", ::(s_type_path(c.cl_path), ::(cf.cf_name, [])));
			case FClosure(co, cf): tag("FClosure", extra_tabs = "\t", ::(switch (co) {
			case None: "None";
			case Some(c, tl): s_type(TInst(c, tl));
				}, ::(cf.cf_name, [])));
			case FAnon(cf): tag("FAnon", extra_tabs = "\t", ::(cf.cf_name, []));
			case FDynamic(s): tag("FDynamic", extra_tabs = "\t", ::(s, []));
			case FEnum(en, ef): tag("FEnum", extra_tabs = "\t", ::(s_type_path(en.e_path), ::(ef.ef_name, [])));
			};
			tag("Field", ::(loop(e1), ::(sfa, [])));
		case TTypeExpr(mt): module_type(mt);
		case TParenthesis(e1): tag("Parenthesis", ::(loop(e1), []));
		case TObjectDecl(fl): tag("ObjectDecl", List.map(function (s, e): sprintf("%s: %s", s, loop(e)), fl));
		case TArrayDecl(el): tag("ArrayDecl", List.map(loop, el));
		case TCall(e1, el): tag("Call", ::(loop(e1), List.map(loop, el)));
		case TNew(c, tl, el): tag("New", ::(s_type(TInst(c, tl)), List.map(loop, el)));
		case TFunction(f): 	function arg(Tuple(v, cto)) return {
				tag("Arg", t = Some(v.v_type), extra_tabs = "\t", switch (cto) {
			case None: ::(local(v), []);
				case Some(ct): ::(local(v), ::(const(ct), []));
				});
			};
			tag("Function", @(List.map(arg, f.tf_args), ::(loop(f.tf_expr), [])));
		case TVar(v, eo): var(v, switch (eo) {
		case None: [];
			case Some(e): ::(loop(e), []);
			});
		case TBlock(el): tag("Block", List.map(loop, el));
		case TIf(e, e1, e2): tag("If", ::(loop(e), ::(Printf.sprintf("[Then:%s] %s", s_type(e1.etype), loop(e1)), switch (e2) {
		case None: [];
			case Some(e): ::(Printf.sprintf("[Else:%s] %s", s_type(e.etype), loop(e)), []);
			})));
		case TCast(e1, None): tag("Cast", ::(loop(e1), []));
		case TCast(e1, Some(mt)): tag("Cast", ::(loop(e1), ::(module_type(mt), [])));
		case TThrow(e1): tag("Throw", ::(loop(e1), []));
		case TBreak: tag("Break", []);
		case TContinue: tag("Continue", []);
		case TReturn(None): tag("Return", []);
		case TReturn(Some(e1)): tag("Return", ::(loop(e1), []));
		case TWhile(e1, e2, NormalWhile): tag("While", ::(loop(e1), ::(loop(e2), [])));
		case TWhile(e1, e2, DoWhile): tag("Do", ::(loop(e1), ::(loop(e2), [])));
		case TFor(v, e1, e2): tag("For", ::(local(v), ::(loop(e1), ::(loop(e2), []))));
		case TTry(e1, catches): var sl = List.map(function (v, e): sprintf("Catch %s%s", local(v), tag_args( ^ (tabs, "\t"),
											 ::(loop(extra_tabs = "\t", e), []))), catches);
			tag("Try", ::(loop(e1), sl));
		case TSwitch(e1, cases, eo): var sl = List.map(function (el, e): tag("Case", t = Some(e.etype), extra_tabs = "\t",
												  @(List.map(loop, el), ::(loop(extra_tabs = "\t", e), []))), cases);
			var sl = switch (eo) {
			case None: sl;
			case Some(e): @(sl, ::(tag("Default", t = Some(e.etype), extra_tabs = "\t", ::(loop(extra_tabs = "\t", e), [])), []));
			};
			tag("Switch", ::(loop(e1), sl));
		case TMeta((m, el, _), e1): var s = Meta.to_string(m);
			var s = switch (el) {
			case []: s;
			case _: sprintf("%s[%s]", s, String.concat(", ", List.map(Ast.s_expr, el)));
			};
			tag("Meta", ::(s, ::(loop(e1), [])));
		};
	};

	public static function s_types( ? : (sep = ", "), tl) return {
		var pctx = print_context([]);
		String.concat(sep, List.map(s_type(pctx), tl));
	};

	public static function s_class_kind(match) return switch (match) {
	case KNormal: "KNormal";
	case KTypeParameter(tl): Printf.sprintf("KTypeParameter [%s]", s_types(tl));
	case KExtension(c, tl): Printf.sprintf("KExtension %s<%s>", s_type_path(c.cl_path), s_types(tl));
	case KExpr(_): "KExpr";
	case KGeneric: "KGeneric";
	case KGenericInstance(c, tl): Printf.sprintf("KGenericInstance %s<%s>", s_type_path(c.cl_path), s_types(tl));
	case KMacroType: "KMacroType";
	case KGenericBuild(_): "KGenericBuild";
	case KAbstractImpl(a): Printf.sprintf("KAbstractImpl %s", s_type_path(a.a_path));
	};

	public static function link(e, a, b) return {
		function loop(t) return {
			if ( == (t, a)) {
				True;
			} else {
				switch (t) {
				case TMono(t): switch (t.val) {
					case None: False;
					case Some(t): loop(t);
					};
				case TEnum(_, tl): List.exists(loop, tl);
				case TInst(_, tl) | TType(_, tl) | TAbstract(_, tl): List.exists(loop, tl);
				case TFun(tl, t): || (List.exists(function (_, _, t): loop(t), tl), loop(t));
				case TDynamic(t2): if ( == (t, t2)) {
						False;
					} else {
						loop(t2);
					};
				case TLazy(f): loop(f.val([]));
				case TAnon(a): try {
						PMap.iter(function _: function f:
						if (loop(f.cf_type)) {
						raise(Exit);
						} else {
							[];
						}, a.a_fields);
						False;
					} catch (e: Exit) {
						True;
					};
				};
			};
		};
		if (loop(b)) {
			== (follow(b), a);
		} else {
			if ( == (b, t_dynamic)) {
				True;
			} else {
				e.val = Some(b);
				True;
			};
		};
	};

	public static function fast_eq(a, b) return {
		if ( == (a, b)) {
			True;
		} else {
			switch ((new Tuple(a, b))) {
			case (TFun(l1, r1), TFun(l2, r2)) if (=(List.length(l1), List.length(l2))):
				&& (List.for_all2(function (_, _, t1): function (_, _, t2): fast_eq(t1, t2), l1, l2), fast_eq(r1, r2));
			case (TType(t1, l1), TType(t2, l2)): && ( == (t1, t2), List.for_all2(fast_eq, l1, l2));
			case (TEnum(e1, l1), TEnum(e2, l2)): && ( == (e1, e2), List.for_all2(fast_eq, l1, l2));
			case (TInst(c1, l1), TInst(c2, l2)): && ( == (c1, c2), List.for_all2(fast_eq, l1, l2));
			case (TAbstract(a1, l1), TAbstract(a2, l2)): && ( == (a1, a2), List.for_all2(fast_eq, l1, l2));
			case (_, _): False;
			};
		};
	};

	public static function fast_eq_mono(ml, a, b) return {
		if ( == (a, b)) {
			True;
		} else {
			switch ((new Tuple(a, b))) {
			case (TFun(l1, r1), TFun(l2, r2)) if (=(List.length(l1), List.length(l2))):
				&& (List.for_all2(function (_, _, t1): function (_, _, t2): fast_eq_mono(ml, t1, t2), l1, l2), fast_eq_mono(ml, r1, r2));
			case (TType(t1, l1), TType(t2, l2)): && ( == (t1, t2), List.for_all2(fast_eq_mono(ml), l1, l2));
			case (TEnum(e1, l1), TEnum(e2, l2)): && ( == (e1, e2), List.for_all2(fast_eq_mono(ml), l1, l2));
			case (TInst(c1, l1), TInst(c2, l2)): && ( == (c1, c2), List.for_all2(fast_eq_mono(ml), l1, l2));
			case (TAbstract(a1, l1), TAbstract(a2, l2)): && ( == (a1, a2), List.for_all2(fast_eq_mono(ml), l1, l2));
			case (TMono(_), _): List.memq(a, ml);
			case (_, _): False;
			};
		};
	};

	public static function cannot_unify(a, b) return {
		Cannot_unify(a, b);
	};

	public static function invalid_field(n) return {
		Invalid_field_type(n);
	};

	public static function invalid_kind(n, a, b) return {
		Invalid_kind(n, a, b);
	};

	public static function invalid_visibility(n) return {
		Invalid_visibility(n);
	};

	public static function has_no_field(t, n) return {
		Has_no_field(t, n);
	};

	public static function has_extra_field(t, n) return {
		Has_extra_field(t, n);
	};

	public static function error(l) return {
		raise(Unify_error(l));
	};

	public static function has_meta(m, ml) return {
		List.exists(function (m2, _, _): = (m, m2), ml);
	};

	public static function get_meta(m, ml) return {
		List.find(function (m2, _, _): = (m, m2), ml);
	};

	public static var no_meta = [];

	public static function unify_access(a1, a2) return {
		|| ( = (a1, a2), switch ((new Tuple(a1, a2))) {
	case (_, AccNo) | (_, AccNever): True;
		case (AccInline, AccNormal): True;
		case _: False;
		});
	};

	public static function direct_access(match) return switch (match) {
	case AccNo | AccNever | AccNormal | AccInline | AccRequire(_): True;
	case AccResolve | AccCall: False;
	};

	public static function unify_kind(k1, k2) return {
		|| ( = (k1, k2), switch ((new Tuple(k1, k2))) {
	case (Var(v1), Var(v2)): && (unify_access(v1.v_read, v2.v_read), unify_access(v1.v_write, v2.v_write));
		case (Var(v), Method(m)): switch ((new Tuple(v.v_read, v.v_write, m))) {
			case (AccNormal, _, MethNormal): True;
			case (AccNormal, AccNormal, MethDynamic): True;
			case _: False;
			};
		case (Method(m), Var(v)): switch (m) {
			case MethDynamic: && (direct_access(v.v_read), direct_access(v.v_write));
			case MethMacro: False;
			case MethNormal | MethInline: switch ((new Tuple(v.v_read, v.v_write))) {
				case (AccNormal, AccNo | AccNever): True;
				case _: False;
				};
			};
		case (Method(m1), Method(m2)): switch ((new Tuple(m1, m2))) {
			case (MethInline, MethNormal) | (MethDynamic, MethNormal): True;
			case _: False;
			};
		});
	};

	public static var eq_stack = ref([]);

	public static function type_eq(param, a, b) return {
		function can_follow(t) return {
			switch (param) {
			case EqCoreType: False;
			case EqDoNotFollowNull: !(is_null(t));
			case _: True;
			};
		};
		if ( == (a, b)) {
			[];
		} else {
			switch ((new Tuple(a, b))) {
			case (TLazy(f), _): type_eq(param, f.val([]), b);
			case (_, TLazy(f)): type_eq(param, a, f.val([]));
			case (TMono(t), _): switch (t.val) {
				case None: if ( || ( = (param, EqCoreType), !(link(t, a, b)))) {
						error(::(cannot_unify(a, b), []));
					} else {
						[];
					};
				case Some(t): type_eq(param, t, b);
				};
			case (_, TMono(t)): switch (t.val) {
				case None: if ( || ( = (param, EqCoreType), !(link(t, b, a)))) {
						error(::(cannot_unify(a, b), []));
					} else {
						[];
					};
				case Some(t): type_eq(param, a, t);
				};
			case (TType(t1, tl1), TType(t2, tl2)) if (&&(||(==(t1, t2), &&(=(param, EqCoreType), =(t1.t_path, t2.t_path))), =(List.length(tl1), List.length(tl2))))
						: List.iter2(type_eq(param), tl1, tl2);
			case (TType(t, tl), _) if (can_follow(a)): type_eq(param, apply_params(t.t_params, tl, t.t_type), b);
			case (_, TType(t, tl)) if (can_follow(b)):
				if (List.exists(function (a2, b2): && (fast_eq(a, a2), fast_eq(b, b2)), eq_stack.val)) {
					[];
				} else {
					eq_stack.val = ::((new Tuple(a, b)), eq_stack.val);
					try {
						type_eq(param, a, apply_params(t.t_params, tl, t.t_type));
						eq_stack.val = List.tl(eq_stack.val);
					} catch (e: Unify_error(l)) {
						eq_stack.val = List.tl(eq_stack.val);
						error(::(cannot_unify(a, b), l));
					};
				};
			case (TEnum(e1, tl1), TEnum(e2, tl2)): if ( && ( != (e1, e2), !( && ( = (param, EqCoreType), = (e1.e_path, e2.e_path))))) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
				List.iter2(type_eq(param), tl1, tl2);
			case (TInst(c1, tl1), TInst(c2, tl2)): if ( && ( != (c1, c2), && (!(
				&& ( = (param, EqCoreType), = (c1.cl_path, c2.cl_path))), switch ((new Tuple(c1.cl_kind, c2.cl_kind))) {
				case (KExpr(_), KExpr(_)): False;
					case _: True;
					}))) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
				List.iter2(type_eq(param), tl1, tl2);
			case (TFun(l1, r1), TFun(l2, r2)) if (=(List.length(l1), List.length(l2))):
				try {
					type_eq(param, r1, r2);
					List.iter2(function (n, o1, t1): function (_, o2, t2):
					if (<>(o1, o2)) {
					error(::(Not_matching_optional(n), []));
					} else {
						[];
					};
					type_eq(param, t1, t2), l1, l2);
				} catch (e: Unify_error(l)) {
					error(::(cannot_unify(a, b), l));
				};
			case (TDynamic(a), TDynamic(b)): type_eq(param, a, b);
			case (TAbstract(a1, tl1), TAbstract(a2, tl2)): if ( && ( != (a1, a2), !(
							&& ( = (param, EqCoreType), = (a1.a_path, a2.a_path))))) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
				List.iter2(type_eq(param), tl1, tl2);
			case (TAnon(a1), TAnon(a2)): try {
					PMap.iter(function n: function f1:
					try {
						var f2 = PMap.find(n, a2.a_fields);
						if ( && (<>(f1.cf_kind, f2.cf_kind), || ( = (param, EqStrict),
								 || ( = (param, EqCoreType), !(unify_kind(f1.cf_kind, f2.cf_kind)))))) {
							error(::(invalid_kind(n, f1.cf_kind, f2.cf_kind), []));
						} else {
							[];
						};
						try {
							type_eq(param, f1.cf_type, f2.cf_type);
						} catch (e: Unify_error(l)) {
							error(::(invalid_field(n), l));
						};
					} catch (e: Not_found) {
						if (is_closed(a2)) {
							error(::(has_no_field(b, n), []));
						} else {
							[];
						};
						if (!(link(ref(None), b, f1.cf_type))) {
							error(::(cannot_unify(a, b), []));
						} else {
							[];
						};
						a2.a_fields = PMap.add(n, f1, a2.a_fields);
					}, a1.a_fields);
					PMap.iter(function n: function f2:
					if (!(PMap.mem(n, a1.a_fields))) {
					if (is_closed(a1)) {
							error(::(has_no_field(a, n), []));
						} else {
							[];
						};
						if (!(link(ref(None), a, f2.cf_type))) {
							error(::(cannot_unify(a, b), []));
						} else {
							[];
						};
						a1.a_fields = PMap.add(n, f2, a1.a_fields);
					} else {
						[];
					}, a2.a_fields);
				} catch (e: Unify_error(l)) {
					error(::(cannot_unify(a, b), l));
				};
			case (_, _): if ( && ( == (b, t_dynamic), || ( = (param, EqRightDynamic), = (param, EqBothDynamic)))) {
					[];
				} else {
					if ( && ( == (a, t_dynamic), = (param, EqBothDynamic))) {
						[];
					} else {
						error(::(cannot_unify(a, b), []));
					};
				};
			};
		};
	};

	public static function type_iseq(a, b) return {
		try {
			type_eq(EqStrict, a, b);
			True;
		} catch (e: Unify_error(_)) {
			False;
		};
	};

	public static function type_iseq_strict(a, b) return {
		try {
			type_eq(EqDoNotFollowNull, a, b);
			True;
		} catch (e: Unify_error(_)) {
			False;
		};
	};

	public static var unify_stack = ref([]);

	public static var abstract_cast_stack = ref([]);

	public static var unify_new_monos = ref([]);

	public static function unify(a, b) return {
		if ( == (a, b)) {
			[];
		} else {
			switch ((new Tuple(a, b))) {
			case (TLazy(f), _): unify(f.val([]), b);
			case (_, TLazy(f)): unify(a, f.val([]));
			case (TMono(t), _): switch (t.val) {
				case None: if (!(link(t, a, b))) {
						error(::(cannot_unify(a, b), []));
					} else {
						[];
					};
				case Some(t): unify(t, b);
				};
			case (_, TMono(t)): switch (t.val) {
				case None: if (!(link(t, b, a))) {
						error(::(cannot_unify(a, b), []));
					} else {
						[];
					};
				case Some(t): unify(a, t);
				};
			case (TType(t, tl), _): if (!(List.exists(function (a2, b2): && (fast_eq(a, a2), fast_eq(b, b2)), unify_stack.val))) {
					try {
						unify_stack.val = ::((new Tuple(a, b)), unify_stack.val);
						unify(apply_params(t.t_params, tl, t.t_type), b);
						unify_stack.val = List.tl(unify_stack.val);
					} catch (e: Unify_error(l)) {
						unify_stack.val = List.tl(unify_stack.val);
						error(::(cannot_unify(a, b), l));
					};
				} else {
					[];
				};
			case (_, TType(t, tl)): if (!(List.exists(function (a2, b2): && (fast_eq(a, a2), fast_eq(b, b2)), unify_stack.val))) {
					try {
						unify_stack.val = ::((new Tuple(a, b)), unify_stack.val);
						unify(a, apply_params(t.t_params, tl, t.t_type));
						unify_stack.val = List.tl(unify_stack.val);
					} catch (e: Unify_error(l)) {
						unify_stack.val = List.tl(unify_stack.val);
						error(::(cannot_unify(a, b), l));
					};
				} else {
					[];
				};
			case (TEnum(ea, tl1), TEnum(eb, tl2)): if ( != (ea, eb)) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
				unify_type_params(a, b, tl1, tl2);
			case (TAbstract(a1, tl1), TAbstract(a2, tl2)) if (==(a1, a2)):
				try {
					unify_type_params(a, b, tl1, tl2);
				} catch (e: Unify_error(_) = err) {
					try {
						unify_abstracts(a, b, a1, tl1, a2, tl2);
					} catch (e: Unify_error(_)) {
						raise(err);
					};
				};
			case (TAbstract({ a_path = ([], Void) }, _), _) | (_, TAbstract({ a_path = ([], Void) }, _)): error(::(cannot_unify(a,
						b), []));
			case (TAbstract(a1, tl1), TAbstract(a2, tl2)): unify_abstracts(a, b, a1, tl1, a2, tl2);
			case (TInst(c1, tl1), TInst(c2, tl2)): 	function loop(c, tl) return {
					if ( == (c, c2)) {
						unify_type_params(a, b, tl, tl2);
						True;
					} else {
						|| (switch (c.cl_super) {
					case None: False;
					case Some(cs, tls): loop(cs, List.map(apply_params(c.cl_params, tl), tls));
						}, || (List.exists(function (cs, tls): loop(cs, List.map(apply_params(c.cl_params, tl), tls)), c.cl_implements), switch (c.cl_kind) {
					case KTypeParameter(pl): List.exists(function t: switch (follow(t)) {
						case TInst(cs, tls): loop(cs, List.map(apply_params(c.cl_params, tl), tls));
							case _: False;
							}, pl);
						case _: False;
						}));
					};
				};
				if (!(loop(c1, tl1))) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
			case (TFun(l1, r1), TFun(l2, r2)) if (=(List.length(l1), List.length(l2))): var i = ref(0);
				try {
					switch (r2) {
					case TAbstract({ a_path = ([], Void) }, _): incr(i);
					case _: unify(r1, r2);
						incr(i);
					};
					List.iter2(function (_, o1, t1): function (_, o2, t2):
					if ( && (o1, !(o2))) {
					error(::(Cant_force_optional, []));
					} else {
						[];
					};
					unify(t1, t2);
					incr(i), l2, l1);
				} catch (e: Unify_error(l)) {
					var msg = if ( = (i.val, 0)) {
						"Cannot unify return types";
					} else {
						^ ("Cannot unify argument ", string_of_int(i.val));
					};
					error(::(cannot_unify(a, b), ::(Unify_custom(msg), l)));
				};
			case (TInst(c, tl), TAnon(an)): if (PMap.is_empty(an.a_fields)) {
					switch (c.cl_kind) {
					case KTypeParameter(pl): if (!(List.exists(function t:
						switch (follow(t)) {
						case TInst(_) | TAnon(_): True;
							case _: False;
							}, pl))) {
							error(::(cannot_unify(a, b), []));
						} else {
							[];
						};
					case _: [];
					};
				} else {
					[];
				};
				try {
					PMap.iter(function n: function f2: var monos = ref([]);
					function make_type(f) return {
						switch (f.cf_params) {
						case []: f.cf_type;
						case l: var ml = List.map(function _: mk_mono([]), l);
							monos.val = ml;
							apply_params(f.cf_params, ml, f.cf_type);
						};
					};
					var Tuple(_, ft, f1) = try {
						raw_class_field(make_type, c, tl, n);
					} catch (e: Not_found) {
						error(::(has_no_field(a, n), []));
					};
					var ft = apply_params(c.cl_params, tl, ft);
					if (!(unify_kind(f1.cf_kind, f2.cf_kind))) {
					error(::(invalid_kind(n, f1.cf_kind, f2.cf_kind), []));
					} else {
						[];
					};
					if ( && (f2.cf_public, !(f1.cf_public))) {
					error(::(invalid_visibility(n), []));
					} else {
						[];
					};
					var old_monos = unify_new_monos.val;
									unify_new_monos.val = @(monos.val, unify_new_monos.val);
									if (!(List.exists(function (a2, b2):
					&& (fast_eq(b2, f2.cf_type), fast_eq_mono(unify_new_monos.val, ft, a2)), unify_stack.val))) {
					unify_stack.val = ::((new Tuple(ft, f2.cf_type)), unify_stack.val);
						try {
							unify_with_access(ft, f2);
						} catch (e: Unify_error(l)) {
							unify_new_monos.val = old_monos;
							unify_stack.val = List.tl(unify_stack.val);
							error(::(invalid_field(n), l));
						};
						unify_stack.val = List.tl(unify_stack.val);
					} else {
						[];
					};
					unify_new_monos.val = old_monos;
										  List.iter(function f2o:
					if (!(List.exists(function f1o: type_iseq(f1o.cf_type, f2o.cf_type), ::(f1, f1.cf_overloads)))) {
					error(::(Missing_overload(f1, f2o.cf_type), []));
					} else {
						[];
					}, f2.cf_overloads);
					if (!(Meta.has(Meta.MaybeUsed, f1.cf_meta))) {
					f1.cf_meta = ::((new Tuple(Meta.MaybeUsed, [], f1.cf_pos)), f1.cf_meta);
					} else {
						[];
					};
					switch (f1.cf_kind) {
				case Method(MethInline): if ( && (
														  || (c.cl_extern, Meta.has(Meta.Extern, f1.cf_meta)), !(Meta.has(Meta.Runtime, f1.cf_meta)))) {
							error(::(Has_no_runtime_field(a, n), []));
						} else {
							[];
						};
					case _: [];
					}, an.a_fields);
					switch (an.a_status.val) {
					case Opened: an.a_status.val = Closed;
					case Statics(_) | EnumStatics(_) | AbstractStatics(_): error([]);
					case Closed | Extend(_) | Const: [];
					};
				} catch (e: Unify_error(l)) {
					error(::(cannot_unify(a, b), l));
				};
			case (TAnon(a1), TAnon(a2)): unify_anons(a, b, a1, a2);
			case (TAnon(an), TAbstract({ a_path = ([], Class) }, ::(pt, []))):
				switch (an.a_status.val) {
				case Statics(cl): unify(TInst(cl, List.map(function _: mk_mono([]), cl.cl_params)), pt);
				case _: error(::(cannot_unify(a, b), []));
				};
			case (TAnon(an), TAbstract({ a_path = ([], Enum) }, ::(pt, []))):
				switch (an.a_status.val) {
				case EnumStatics(e): unify(TEnum(e, List.map(function _: mk_mono([]), e.e_params)), pt);
				case _: error(::(cannot_unify(a, b), []));
				};
			case (TEnum(_), TAbstract({ a_path = ([], EnumValue) }, [])): [];
			case (TEnum(en, _), TAbstract({ a_path = (::(haxe, []), FlatEnum) }, [])) if (Meta.has(Meta.FlatEnum, en.e_meta)): [];
			case (TFun(_), TAbstract({ a_path = (::(haxe, []), Function) }, [])): [];
			case (TDynamic(t), _): if ( == (t, a)) {
					[];
				} else {
					switch (b) {
					case TDynamic(t2): if ( != (t2, b)) {
							try {
								type_eq(EqRightDynamic, t, t2);
							} catch (e: Unify_error(l)) {
								error(::(cannot_unify(a, b), l));
							};
						} else {
							[];
						};
					case TAbstract(bb, tl) if (List.exists(unify_from(bb, tl, a, b), bb.a_from)): [];
					case _: error(::(cannot_unify(a, b), []));
					};
				};
			case (_, TDynamic(t)): if ( == (t, b)) {
					[];
				} else {
					switch (a) {
					case TDynamic(t2): if ( != (t2, a)) {
							try {
								type_eq(EqRightDynamic, t, t2);
							} catch (e: Unify_error(l)) {
								error(::(cannot_unify(a, b), l));
							};
						} else {
							[];
						};
					case TAnon(an): try {
							switch (an.a_status.val) {
							case Statics(_) | EnumStatics(_): error([]);
							case Opened: an.a_status.val = Closed;
							case _: [];
							};
							PMap.iter(function _: function f:
							try {
								type_eq(EqStrict, field_type(f), t);
							} catch (e: Unify_error(l)) {
								error(::(invalid_field(f.cf_name), l));
							}, an.a_fields);
						} catch (e: Unify_error(l)) {
							error(::(cannot_unify(a, b), l));
						};
					case TAbstract(aa, tl) if (List.exists(unify_to(aa, tl, b), aa.a_to)): [];
					case _: error(::(cannot_unify(a, b), []));
					};
				};
			case (TAbstract(aa, tl), _): if (!(List.exists(unify_to(aa, tl, b), aa.a_to))) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
			case (TInst({ cl_kind = KTypeParameter(ctl) } = c, pl), TAbstract(bb, tl)):
				if ( && (!(List.exists(function t: var t = apply_params(c.cl_params, pl, t);
				try {
					unify(t, b);
						True;
					} catch (e: Unify_error(_)) {
						False;
					}, ctl)), !(List.exists(unify_from(bb, tl, a, b), bb.a_from)))) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
			case (_, TAbstract(bb, tl)): if (!(List.exists(unify_from(bb, tl, a, b), bb.a_from))) {
					error(::(cannot_unify(a, b), []));
				} else {
					[];
				};
			case (_, _): error(::(cannot_unify(a, b), []));
			};
		};
	};

	public static function unify_abstracts(a, b, a1, tl1, a2, tl2) return {
		var f1 = unify_to(a1, tl1, b);
		var f2 = unify_from(a2, tl2, a, b);
		if ( || (List.exists(f1(allow_transitive_cast = False), a1.a_to), || (List.exists(f2(allow_transitive_cast = False), a2.a_from), && ( || (Meta.has(Meta.CoreType, a1.a_meta), Meta.has(Meta.CoreType, a2.a_meta)), || (List.exists(f1, a1.a_to), List.exists(f2, a2.a_from)))))) {
			[];
		} else {
			error(::(cannot_unify(a, b), []));
		};
	};

	public static function unify_anons(a, b, a1, a2) return {
		try {
			PMap.iter(function n: function f2:
			try {
				var f1 = PMap.find(n, a1.a_fields);
				if (!(unify_kind(f1.cf_kind, f2.cf_kind))) {
					switch ((new Tuple(a1.a_status.val, f1.cf_kind, f2.cf_kind))) {
					case (Opened, Var({ v_read = AccNormal; v_write = AccNo }), Var({ v_read = AccNormal; v_write = AccNormal })): f1.cf_kind =
						f2.cf_kind;
					case _: error(::(invalid_kind(n, f1.cf_kind, f2.cf_kind), []));
					};
				} else {
					[];
				};
				if ( && (f2.cf_public, !(f1.cf_public))) {
					error(::(invalid_visibility(n), []));
				} else {
					[];
				};
				try {
					unify_with_access(f1.cf_type, f2);
					switch (a1.a_status.val) {
					case Statics(c) if (!(Meta.has(Meta.MaybeUsed, f1.cf_meta))): f1.cf_meta = ::((new Tuple(Meta.MaybeUsed, [], f1.cf_pos)),
						f1.cf_meta);
					case _: [];
					};
				} catch (e: Unify_error(l)) {
					error(::(invalid_field(n), l));
				};
			} catch (e: Not_found) {
				switch (a1.a_status.val) {
				case Opened: if (!(link(ref(None), a, f2.cf_type))) {
						error([]);
					} else {
						[];
					};
					a1.a_fields = PMap.add(n, f2, a1.a_fields);
				case Const if (Meta.has(Meta.Optional, f2.cf_meta)): [];
				case _: error(::(has_no_field(a, n), []));
				};
			}, a2.a_fields);
			switch (a1.a_status.val) {
			case Const if (!(PMap.is_empty(a2.a_fields))): PMap.iter(function n: function _:
				if (!(PMap.mem(n, a2.a_fields))) {
				error(::(has_extra_field(a, n), []));
				} else {
					[];
				}, a1.a_fields);
			case Opened: a1.a_status.val = Closed;
			case _: [];
			};
			switch (a2.a_status.val) {
			case Statics(c): switch (a1.a_status.val) {
				case Statics(c2) if (==(c, c2)): [];
				case _: error([]);
				};
			case EnumStatics(e): switch (a1.a_status.val) {
				case EnumStatics(e2) if (==(e, e2)): [];
				case _: error([]);
				};
			case AbstractStatics(a): switch (a1.a_status.val) {
				case AbstractStatics(a2) if (==(a, a2)): [];
				case _: error([]);
				};
			case Opened: a2.a_status.val = Closed;
			case Const | Extend(_) | Closed: [];
			};
		} catch (e: Unify_error(l)) {
			error(::(cannot_unify(a, b), l));
		};
	};

	public static function unify_from(ab, tl, a, b, ? : (allow_transitive_cast = True), t) return {
		if (List.exists(function (a2, b2): && (fast_eq(a, a2), fast_eq(b, b2)), abstract_cast_stack.val)) {
			False;
		} else {
			abstract_cast_stack.val = ::((new Tuple(a, b)), abstract_cast_stack.val);
			var t = apply_params(ab.a_params, tl, t);
			var unify_func = if (allow_transitive_cast) {
				unify;
			} else {
				type_eq(EqStrict);
			};
			var b = try {
				unify_func(a, t);
				True;
			} catch (e: Unify_error(_)) {
				False;
			};
			abstract_cast_stack.val = List.tl(abstract_cast_stack.val);
			b;
		};
	};

	public static function unify_to(ab, tl, b, ? : (allow_transitive_cast = True), t) return {
		var t = apply_params(ab.a_params, tl, t);
		var unify_func = if (allow_transitive_cast) {
			unify;
		} else {
			type_eq(EqStrict);
		};
		try {
			unify_func(t, b);
			True;
		} catch (e: Unify_error(_)) {
			False;
		};
	};

	public static function unify_from_field(ab, tl, a, b, ? : (allow_transitive_cast = True), Tuple(t, cf)) return {
		if (List.exists(function (a2, b2): && (fast_eq(a, a2), fast_eq(b, b2)), abstract_cast_stack.val)) {
			False;
		} else {
			abstract_cast_stack.val = ::((new Tuple(a, b)), abstract_cast_stack.val);
			var unify_func = if (allow_transitive_cast) {
				unify;
			} else {
				type_eq(EqStrict);
			};
			var b = try {
				switch (follow(cf.cf_type)) {
				case TFun(_, r): var monos = List.map(function _: mk_mono([]), cf.cf_params);
					function map(t) return {
						apply_params(ab.a_params, tl, apply_params(cf.cf_params, monos, t));
					};
					unify_func(a, map(t));
					List.iter2(function m: function (name, t):
					switch (follow(t)) {
				case TInst({ cl_kind = KTypeParameter(constr) }, _) if (<>(constr, [])): List.iter(function tc:
						switch (follow(m)) {
					case TMono(_): raise(Unify_error([]));
						case _: unify(m, map(tc));
						}, constr);
					case _: [];
					}, monos, cf.cf_params);
					unify_func(map(r), b);
				case _: assert False;
				};
				True;
			} catch (e: Unify_error(_)) {
				False;
			};
			abstract_cast_stack.val = List.tl(abstract_cast_stack.val);
			b;
		};
	};

	public static function unify_to_field(ab, tl, b, ? : (allow_transitive_cast = True), Tuple(t, cf)) return {
		var a = TAbstract(ab, tl);
		if (List.exists(function (b2, a2): && (fast_eq(a, a2), fast_eq(b, b2)), abstract_cast_stack.val)) {
			False;
		} else {
			abstract_cast_stack.val = ::((new Tuple(b, a)), abstract_cast_stack.val);
			var unify_func = if (allow_transitive_cast) {
				unify;
			} else {
				type_eq(EqStrict);
			};
			var r = try {
				switch (follow(cf.cf_type)) {
				case TFun(::((_, _, ta), _), _): var monos = List.map(function _: mk_mono([]), cf.cf_params);
					function map(t) return {
						apply_params(ab.a_params, tl, apply_params(cf.cf_params, monos, t));
					};
					var athis = map(ab.a_this);
					with_variance(type_eq(EqStrict), athis, map(ta));
					List.iter2(function m: function (name, t):
					switch (follow(t)) {
				case TInst({ cl_kind = KTypeParameter(constr) }, _) if (<>(constr, [])): List.iter(function tc:
						switch (follow(m)) {
					case TMono(_): raise(Unify_error([]));
						case _: unify(m, map(tc));
						}, constr);
					case _: [];
					}, monos, cf.cf_params);
					unify_func(map(t), b);
				case _: assert False;
				};
				True;
			} catch (e: Unify_error(_)) {
				False;
			};
			abstract_cast_stack.val = List.tl(abstract_cast_stack.val);
			r;
		};
	};

	public static function unify_with_variance(f, t1, t2) return {
		function allows_variance_to(t, tf) return {
			type_iseq(tf, t);
		};
		switch ((new Tuple(follow(t1), follow(t2)))) {
		case (TInst(c1, tl1), TInst(c2, tl2)) if (==(c1, c2)): List.iter2(f, tl1, tl2);
		case (TEnum(en1, tl1), TEnum(en2, tl2)) if (==(en1, en2)): List.iter2(f, tl1, tl2);
		case (TAbstract(a1, tl1), TAbstract(a2, tl2)) if (&&(==(a1, a2), Meta.has(Meta.CoreType, a1.a_meta))): List.iter2(f, tl1,
					tl2);
		case (TAbstract(a1, pl1), TAbstract(a2, pl2)): if (
				&& (Meta.has(Meta.CoreType, a1.a_meta), Meta.has(Meta.CoreType, a2.a_meta))) {
				var ta1 = apply_params(a1.a_params, pl1, a1.a_this);
				var ta2 = apply_params(a2.a_params, pl2, a2.a_this);
				type_eq(EqStrict, ta1, ta2);
			} else {
				[];
			};
			if ( && (!(List.exists(allows_variance_to(t2), a1.a_to)), !(List.exists(allows_variance_to(t1), a2.a_from)))) {
				error(::(cannot_unify(t1, t2), []));
			} else {
				[];
			};
		case (TAbstract(a, pl), t): type_eq(EqBothDynamic, apply_params(a.a_params, pl, a.a_this), t);
			if (!(List.exists(function t2: allows_variance_to(t, apply_params(a.a_params, pl, t2)), a.a_to))) {
				error(::(cannot_unify(t1, t2), []));
			} else {
				[];
			};
		case (t, TAbstract(a, pl)): type_eq(EqBothDynamic, t, apply_params(a.a_params, pl, a.a_this));
			if (!(List.exists(function t2: allows_variance_to(t, apply_params(a.a_params, pl, t2)), a.a_from))) {
				error(::(cannot_unify(t1, t2), []));
			} else {
				[];
			};
		case (TAnon(a1), TAnon(a2)): unify_anons(t1, t2, a1, a2);
		case _: error(::(cannot_unify(t1, t2), []));
		};
	};

	public static function unify_type_params(a, b, tl1, tl2) return {
		List.iter2(function t1: function t2:
		try {
			with_variance(type_eq(EqRightDynamic), t1, t2);
		} catch (e: Unify_error(l)) {
			var err = cannot_unify(a, b);
			error(::(err, ::(Invariant_parameter(t1, t2), l)));
		}, tl1, tl2);
	};

	public static function with_variance(f, t1, t2) return {
		try {
			f(t1, t2);
		} catch (e: Unify_error(l)) {
			try {
				unify_with_variance(with_variance(f), t1, t2);
			} catch (e: Unify_error(_)) {
				raise(Unify_error(l));
			};
		};
	};

	public static function unify_with_access(t1, f2) return {
		switch (f2.cf_kind) {
		case Var({ v_read = AccNo }) | Var({ v_read = AccNever }): unify(f2.cf_type, t1);
		case Method(MethNormal) | Method(MethInline) | Var({ v_write = AccNo }) | Var({ v_write = AccNever }): unify(t1,
					f2.cf_type);
		case _: with_variance(type_eq(EqBothDynamic), t1, f2.cf_type);
		};
	};

	public static function iter_dt(f, dt) return {
		switch (dt) {
		case DTBind(_, dt): f(dt);
		case DTSwitch(_, cl, dto): List.iter(function (_, dt): f(dt), cl);
			switch (dto) {
			case None: [];
			case Some(dt): f(dt);
			};
		case DTGuard(_, dt1, dt2): f(dt1);
			switch (dt2) {
			case None: [];
			case Some(dt): f(dt);
			};
		case DTGoto(_) | DTExpr(_): [];
		};
	};

	public static function iter(f, e) return {
		switch (e.eexpr) {
		case TConst(_) | TLocal(_) | TBreak | TContinue | TTypeExpr(_): [];
		case TArray(e1, e2) | TBinop(_, e1, e2) | TFor(_, e1, e2) | TWhile(e1, e2, _): f(e1);
			f(e2);
		case TThrow(e) | TField(e, _) | TEnumParameter(e, _, _) | TParenthesis(e) | TCast(e, _) | TUnop(_, _, e) | TMeta(_, e): f(
				e);
		case TArrayDecl(el) | TNew(_, _, el) | TBlock(el): List.iter(f, el);
		case TObjectDecl(fl): List.iter(function (_, e): f(e), fl);
		case TCall(e, el): f(e);
			List.iter(f, el);
		case TVar(v, eo): switch (eo) {
			case None: [];
			case Some(e): f(e);
			};
		case TFunction(fu): f(fu.tf_expr);
		case TIf(e, e1, e2): f(e);
			f(e1);
			switch (e2) {
			case None: [];
			case Some(e): f(e);
			};
		case TSwitch(e, cases, def): f(e);
			List.iter(function (el, e2): List.iter(f, el);
					  f(e2), cases);
			switch (def) {
			case None: [];
			case Some(e): f(e);
			};
		case TTry(e, catches): f(e);
			List.iter(function (_, e): f(e), catches);
		case TReturn(eo): switch (eo) {
			case None: [];
			case Some(e): f(e);
			};
		};
	};

	public static function map_expr(f, e) return {
		switch (e.eexpr) {
		case TConst(_) | TLocal(_) | TBreak | TContinue | TTypeExpr(_): e;
		case TArray(e1, e2): var e1 = f(e1);
			{
				(e) with eexpr = TArray(e1, f(e2))
			};
		case TBinop(op, e1, e2): var e1 = f(e1);
			{
				(e) with eexpr = TBinop(op, e1, f(e2))
			};
		case TFor(v, e1, e2): var e1 = f(e1);
			{
				(e) with eexpr = TFor(v, e1, f(e2))
			};
		case TWhile(e1, e2, flag): var e1 = f(e1);
			{
				(e) with eexpr = TWhile(e1, f(e2), flag)
			};
		case TThrow(e1): {
			(e) with eexpr = TThrow(f(e1))
		};
		case TEnumParameter(e1, ef, i): {
			(e) with eexpr = TEnumParameter(f(e1), ef, i)
		};
		case TField(e1, v): {
			(e) with eexpr = TField(f(e1), v)
		};
		case TParenthesis(e1): {
			(e) with eexpr = TParenthesis(f(e1))
		};
		case TUnop(op, pre, e1): {
			(e) with eexpr = TUnop(op, pre, f(e1))
		};
		case TArrayDecl(el): {
			(e) with eexpr = TArrayDecl(List.map(f, el))
		};
		case TNew(t, pl, el): {
			(e) with eexpr = TNew(t, pl, List.map(f, el))
		};
		case TBlock(el): {
			(e) with eexpr = TBlock(List.map(f, el))
		};
		case TObjectDecl(el): {
			(e) with eexpr = TObjectDecl(List.map(function (v, e): (new Tuple(v, f(e))), el))
		};
		case TCall(e1, el): {
			(e) with eexpr = TCall(f(e1), List.map(f, el))
		};
		case TVar(v, eo): {
			(e) with eexpr = TVar(v, switch (eo) {
		case None: None;
		case Some(e): Some(f(e));
			})
		};
		case TFunction(fu): {
			(e) with eexpr = TFunction({ (fu) with tf_expr = f(fu.tf_expr) })
		};
		case TIf(ec, e1, e2): var ec = f(ec);
			var e1 = f(e1);
			{
				(e) with eexpr = TIf(ec, e1, switch (e2) {
			case None: None;
			case Some(e): Some(f(e));
				})
			};
		case TSwitch(e1, cases, def): var e1 = f(e1);
			var cases = List.map(function (el, e2): (new Tuple(List.map(f, el), f(e2))), cases);
			{
				(e) with eexpr = TSwitch(e1, cases, switch (def) {
			case None: None;
			case Some(e): Some(f(e));
				})
			};
		case TTry(e1, catches): var e1 = f(e1);
			{
				(e) with eexpr = TTry(e1, List.map(function (v, e): (new Tuple(v, f(e))), catches))
			};
		case TReturn(eo): {
			(e) with eexpr = TReturn(switch (eo) {
		case None: None;
		case Some(e): Some(f(e));
			})
		};
		case TCast(e1, t): {
			(e) with eexpr = TCast(f(e1), t)
		};
		case TMeta(m, e1): {
			(e) with eexpr = TMeta(m, f(e1))
		};
		};
	};

	public static function map_expr_type(f, ft, fv, e) return {
		switch (e.eexpr) {
		case TConst(_) | TBreak | TContinue | TTypeExpr(_): {
			(e) with etype = ft(e.etype)
		};
		case TLocal(v): {
			(e) with eexpr = TLocal(fv(v));
			etype = ft(e.etype)
		};
		case TArray(e1, e2): var e1 = f(e1);
			{
				(e) with eexpr = TArray(e1, f(e2));
				etype = ft(e.etype)
			};
		case TBinop(op, e1, e2): var e1 = f(e1);
			{
				(e) with eexpr = TBinop(op, e1, f(e2));
				etype = ft(e.etype)
			};
		case TFor(v, e1, e2): var v = fv(v);
			var e1 = f(e1);
			{
				(e) with eexpr = TFor(v, e1, f(e2));
				etype = ft(e.etype)
			};
		case TWhile(e1, e2, flag): var e1 = f(e1);
			{
				(e) with eexpr = TWhile(e1, f(e2), flag);
				etype = ft(e.etype)
			};
		case TThrow(e1): {
			(e) with eexpr = TThrow(f(e1));
			etype = ft(e.etype)
		};
		case TEnumParameter(e1, ef, i): {
			(e) with eexpr = TEnumParameter(f(e1), ef, i);
			etype = ft(e.etype)
		};
		case TField(e1, v): var e1 = f(e1);
			var v = try {
				var n = switch (v) {
				case FClosure(_): raise(Not_found);
				case FAnon(f) | FInstance(_, _, f) | FStatic(_, f): f.cf_name;
				case FEnum(_, f): f.ef_name;
				case FDynamic(n): n;
				};
				quick_field(e1.etype, n);
			} catch (e: Not_found) {
				v;
			};
			{
				(e) with eexpr = TField(e1, v);
				etype = ft(e.etype)
			};
		case TParenthesis(e1): {
			(e) with eexpr = TParenthesis(f(e1));
			etype = ft(e.etype)
		};
		case TUnop(op, pre, e1): {
			(e) with eexpr = TUnop(op, pre, f(e1));
			etype = ft(e.etype)
		};
		case TArrayDecl(el): {
			(e) with eexpr = TArrayDecl(List.map(f, el));
			etype = ft(e.etype)
		};
		case TNew(c, pl, el): var et = ft(e.etype);
			var t = switch (c.cl_kind) {
			case KTypeParameter(_) | KGeneric: et;
			case _: ft(TInst(c, pl));
			};
			var Tuple(c, pl) = switch (follow(t)) {
			case TInst(c, pl): (new Tuple(c, pl));
			case TAbstract({ a_impl = Some(c) }, pl): (new Tuple(c, pl));
			case t: error(::(has_no_field(t, "new"), []));
			};
			{
				(e) with eexpr = TNew(c, pl, List.map(f, el));
				etype = et
			};
		case TBlock(el): {
			(e) with eexpr = TBlock(List.map(f, el));
			etype = ft(e.etype)
		};
		case TObjectDecl(el): {
			(e) with eexpr = TObjectDecl(List.map(function (v, e): (new Tuple(v, f(e))), el));
			etype = ft(e.etype)
		};
		case TCall(e1, el): var e1 = f(e1);
			{
				(e) with eexpr = TCall(e1, List.map(f, el));
				etype = ft(e.etype)
			};
		case TVar(v, eo): {
			(e) with eexpr = TVar(fv(v), switch (eo) {
		case None: None;
		case Some(e): Some(f(e));
			});
			etype = ft(e.etype)
		};
		case TFunction(fu): var fu = { () with tf_expr = f(fu.tf_expr);
										   tf_args = List.map(function (v, o): (new Tuple(fv(v), o)), fu.tf_args);
										   tf_type = ft(fu.tf_type)
										 };
			{
				(e) with eexpr = TFunction(fu);
				etype = ft(e.etype)
			};
		case TIf(ec, e1, e2): var ec = f(ec);
			var e1 = f(e1);
			{
				(e) with eexpr = TIf(ec, e1, switch (e2) {
			case None: None;
			case Some(e): Some(f(e));
				});
				etype = ft(e.etype)
			};
		case TSwitch(e1, cases, def): var e1 = f(e1);
			var cases = List.map(function (el, e2): (new Tuple(List.map(f, el), f(e2))), cases);
			{
				(e) with eexpr = TSwitch(e1, cases, switch (def) {
			case None: None;
			case Some(e): Some(f(e));
				});
				etype = ft(e.etype)
			};
		case TTry(e1, catches): var e1 = f(e1);
			{
				(e) with eexpr = TTry(e1, List.map(function (v, e): (new Tuple(fv(v), f(e))), catches));
				etype = ft(e.etype)
			};
		case TReturn(eo): {
			(e) with eexpr = TReturn(switch (eo) {
		case None: None;
		case Some(e): Some(f(e));
			});
			etype = ft(e.etype)
		};
		case TCast(e1, t): {
			(e) with eexpr = TCast(f(e1), t);
			etype = ft(e.etype)
		};
		case TMeta(m, e1): {
			(e) with eexpr = TMeta(m, f(e1));
			etype = ft(e.etype)
		};
		};
	};

	public static function print_if(b, e) return {
		if (b) {
			print_endline(s_expr_pretty("", s_type(print_context([])), e));
		} else {
			[];
		};
	}
}
;
