import Ast;
import Type;
import As3;
import As3hl;
import Common;

enum Read {
	Read;
};

enum Write {
	Unused__;
	Write;
};

enum Tkind {
	KInt;
	KUInt;
	KFloat;
	KBool;
	KType(value: Hl_name);
	KDynamic;
	KNone;
};

typedef Register = {
	rid : Int,
	rtype : Tkind,
	rused : Bool,
	rinit : Bool,
	rcond : Bool
};

enum Access {
	VReg(value: Register);
	VId(value: Hl_name);
	VCast(value: Hl_nameTkind);
	VGlobal(value: Hl_name);
	VArray;
	VScope(value: Hl_slot);
	VVolatile(value: Hl_nameOption<Tkind>);
	VSuper(value: Hl_name);
};

enum Local {
	LReg(value: Register);
	LScope(value: Hl_slot);
	LGlobal(value: Hl_name);
};

typedef Code_infos = {
	iregs : DynArray<Register>,
	ipos : Int,
	istack : Int,
	imax : Int,
	iscopes : Int,
	imaxscopes : Int,
	iloop : Int,
	icond : Bool
};

typedef Try_infos = {
	tr_pos : Int,
	tr_end : Int,
	tr_catch_pos : Int,
	tr_type : T
};

typedef Context = {
	com : Common.Context,
	debugger : Bool,
	swc : Bool,
	boot : Path,
	swf_protected : Bool,
	need_ctor_skip : Bool,
	cur_class : Tclass,
	debug : Bool,
	last_line : Int,
	last_file : String,
	locals : PMap<Int, Tuple<Tvar, Local>>,
	code : DynArray<Hl_opcode>,
	infos : Code_infos,
	trys : List<Try_infos>,
	breaks : List < Unit -> Unit >,
	continues : List < Int -> Unit >,
	in_static : Bool,
	block_vars : List<Tuple<Hl_slot, String, Option<Hl_name>>>,
	try_scope_reg : Option<Register>,
	for_call : Bool
};

class Genswf9 {
	public static function follow(t) return {
		switch (Type.follow(t)) {
		case TAbstract(a, tl) if (!(Meta.has(Meta.CoreType, a.a_meta))): follow(Abstract.get_underlying_type(a, tl));
		case t: t;
		};
	};

	public static function invalid_expr(p) return {
		error("Invalid expression", p);
	};

	public static function stack_error(p) return {
		error("Stack error", p);
	};

	public static function index_int(xint) return {
		Obj.magic(+(x, 1)) : index(a);
	};

	public static function index_nz_int(xint) return {
		Obj.magic(x) : index_nz(a);
	};

	public static function tid(xindex(a)) return {
		Obj.magic(x) : int;
	};

	public static var ethis = mk(TConst(TThis), mk_mono([]), null_pos);

	public static var dynamic_prop = HMMultiNameLate(::(HNPublic(Some("")), []));

	public static function is_special_compare(e1, e2) return {
		switch ((new Tuple(e1.eexpr, e2.eexpr))) {
		case (TConst(TNull), _) | (_, TConst(TNull)): None;
		case _: switch ((new Tuple(follow(e1.etype), follow(e2.etype)))) {
			case (TInst({ cl_path = (::(flash, []), NativeXml) } = c, _), _) | (_, TInst({ cl_path = (::(flash, []), NativeXml) } = c, _))
				: Some(c);
			case _: None;
			};
		};
	};

	public static function write(ctx, op) return {
		DynArray.add(ctx.code, op);
		ctx.infos.ipos = +(ctx.infos.ipos, 1);
		var s = +(ctx.infos.istack, As3hlparse.stack_delta(op));
		ctx.infos.istack = s;
		if ( > (s, ctx.infos.imax)) {
			ctx.infos.imax = s;
		} else {
			[];
		};
		switch (op) {
		case HScope: var n = +(ctx.infos.iscopes, 1);
			ctx.infos.iscopes = n;
			if ( > (n, ctx.infos.imaxscopes)) {
				ctx.infos.imaxscopes = n;
			} else {
				[];
			};
		case HPopScope: ctx.infos.iscopes = -(ctx.infos.iscopes, 1);
		case _: [];
		};
	};

	public static function jump(ctx, cond) return {
		var op = DynArray.length(ctx.code);
		var p = ctx.infos.ipos;
		write(ctx, HJump(cond, 0));
		function []: var delta = -(ctx.infos.ipos, p);
		DynArray.set(ctx.code, op, HJump(cond, delta));
	};

	public static function jump_back(ctx) return {
		var p = ctx.infos.ipos;
		write(ctx, HLabel);
		function cond: var delta = -(p, ctx.infos.ipos);
		write(ctx, HJump(cond, delta));
	};

	public static function real_path(match) return switch (match) {
	case ([], Int): (new Tuple([], "int"));
	case ([], UInt): (new Tuple([], "uint"));
	case ([], Float): (new Tuple([], "Number"));
	case ([], Bool): (new Tuple([], "Boolean"));
	case ([], Enum): (new Tuple([], "Class"));
	case ([], EnumValue): (new Tuple([], "Object"));
	case (::(flash, ::(xml, [])), XML): (new Tuple([], "XML"));
	case (::(flash, ::(xml, [])), XMLList): (new Tuple([], "XMLList"));
	case (::(flash, ::(utils, [])), QName): (new Tuple([], "QName"));
	case (::(flash, ::(utils, [])), Namespace): (new Tuple([], "Namespace"));
	case (::(flash, ::(utils, [])), Object): (new Tuple([], "Object"));
	case (::(flash, ::(utils, [])), Function): (new Tuple([], "Function"));
	case (::(flash, []), FlashXml__): (new Tuple([], "Xml"));
	case (::(flash, ::(errors, [])), Error): (new Tuple([], "Error"));
	case (::(flash, []), Vector): (new Tuple(::("__AS3__", ::("vec", [])), "Vector"));
	case path: path;
	};

	public static function type_path(ctx, path) return {
		var Tuple(pack, name) = real_path(path);
		HMPath(pack, name);
	};

	public static function follow_basic(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case Some(t): follow_basic(t);
			case _: t;
			};
		case TLazy(f): follow_basic(f.val([]));
		case TType({ t_path = ([], Null) }, ::(tp, [])):
			switch (follow_basic(tp)) {
			case TMono(_) | TFun(_) | TAbstract({ a_path = ([], Int) }, []) | TAbstract({ a_path = ([], Float) }, []) | TAbstract({ a_path = ([], UInt) }, []) | TAbstract({ a_path = ([], Bool) }, []) | TInst({ cl_path = (::(haxe, []), Int32) }, [])
					: t;
			case t: t;
			};
		case TType({ t_path = (::(flash, ::(utils, [])), Object) }, []) | TType({ t_path = (::(flash, ::(utils, [])), Function) }, []) | TType({ t_path = ([], UInt) }, [])
				: t;
		case TType(t, tl): follow_basic(apply_params(t.t_params, tl, t.t_type));
		case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): follow_basic(apply_params(a.a_params, pl, a.a_this));
		case _: t;
		};
	};

	public static function type_id(ctx, t) return {
		switch (follow_basic(t)) {
		case TInst({ cl_path = (::(haxe, []), Int32) }, _): type_path(ctx, (new Tuple([], "Int")));
		case TInst({ cl_path = (::(flash, []), Vector) } = c, pl):
			switch (pl) {
			case ::(TInst({ cl_kind = KTypeParameter(_) }, _), []): type_path(ctx, (new Tuple([], "Object")));
			case _: HMParams(type_path(ctx, c.cl_path), List.map(type_id(ctx), pl));
			};
		case TInst(c, _): switch (c.cl_kind) {
			case KTypeParameter(l): switch (l) {
				case ::(t, []): type_id(ctx, t);
				case _: type_path(ctx, (new Tuple([], "Object")));
				};
			case KExtension(c, params): type_id(ctx, TInst(c, params));
			case _: type_path(ctx, c.cl_path);
			};
		case TAbstract(a, _): type_path(ctx, a.a_path);
		case TFun(_) | TType({ t_path = (::(flash, ::(utils, [])), Function) }, []): type_path(ctx, (new Tuple([], "Function")));
		case TType({ t_path = ([], UInt) = path }, _): type_path(ctx, path);
		case TEnum({ e_path = (::(flash, []), XmlType); e_extern = True }, _): HMPath([], "String");
		case TEnum(e, _): function loop(match) return switch (match) {
			case []: type_path(ctx, e.e_path);
			case ::((Meta.FakeEnum, ::((Ast.EConst(Ast.Ident(n)), _), []), _), _): type_path(ctx, (new Tuple([], n)));
			case ::(_, l): loop(l);
			};
			loop(e.e_meta);
		case _: HMPath([], "Object");
		};
	};

	public static function type_opt(ctx, t) return {
		switch (follow_basic(t)) {
		case TDynamic(_) | TMono(_): None;
		case _: Some(type_id(ctx, t));
		};
	};

	public static function type_void(ctx, t) return {
		switch (follow(t)) {
		case TEnum({ e_path = ([], Void) }, _) | TAbstract({ a_path = ([], Void) }, _): Some(HMPath([], "void"));
		case _: type_opt(ctx, t);
		};
	};

	public static function classify(ctx, t) return {
		switch (follow_basic(t)) {
		case TAbstract({ a_path = ([], Int) }, _) | TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = (::(haxe, []), Int32) }, _)
			: KInt;
		case TAbstract({ a_path = ([], Float) }, _) | TInst({ cl_path = ([], Float) }, _): KFloat;
		case TAbstract({ a_path = ([], Bool) }, _) | TEnum({ e_path = ([], Bool) }, _): KBool;
		case TAbstract({ a_path = ([], Void) }, _) | TEnum({ e_path = ([], Void) }, _): KDynamic;
		case TEnum({ e_path = (::(flash, []), XmlType); e_extern = True }, _): KType(HMPath([], "String"));
		case TEnum(e, _): function loop(match) return switch (match) {
			case []: KType(type_id(ctx, t));
			case ::((Meta.FakeEnum, ::((Ast.EConst(Ident(n)), _), []), _), _): switch (n) {
				case Int: KInt;
				case UInt: KUInt;
				case String: KType(HMPath([], "String"));
				case _: assert False;
				};
			case ::(_, l): loop(l);
			};
			loop(e.e_meta);
		case TAbstract({ a_path = ([], UInt) }, _) | TType({ t_path = ([], UInt) }, _): KUInt;
		case TFun(_) | TType({ t_path = (::(flash, ::(utils, [])), Function) }, []): KType(HMPath([], "Function"));
		case TAnon(a): switch (a.a_status.val) {
			case Statics(_): KNone;
			case _: KDynamic;
			};
		case TType({ t_path = (::(flash, ::(utils, [])), Object) }, []): KType(HMPath([], "Object"));
		case TInst(_) | TAbstract(_): KType(type_id(ctx, t));
		case TMono(_) | TType(_) | TDynamic(_): KDynamic;
		case TLazy(_): assert False;
		};
	};

	public static function reserved(i) return {
		switch (i) {
		case int: ^ ("_", i);
		case _: i;
		};
	};

	public static function ident(i) return {
		HMPath([], reserved(i));
	};

	public static function as3(p) return {
		HMName(p, HNNamespace("http://adobe.com/AS3/2006/builtin"));
	};

	public static function property(ctx, p, t) return {
		switch (follow(t)) {
		case TInst({ cl_path = ([], Array) }, _):
			switch (p) {
			case length: (new Tuple(ident(p), Some(KInt), False));
			case map | filter if (Common.defined(ctx.com, Define.NoFlashOverride)): (new Tuple(ident( ^ (p, "HX")), None, True));
			case copy | insert | remove | iterator | toString | map | filter: (new Tuple(ident(p), None, True));
			case _: (new Tuple(as3(p), None, False));
			};
		case TInst({ cl_path = (::(flash, []), Vector) }, _):
			switch (p) {
			case length: (new Tuple(ident(p), Some(KInt), False));
			case fixed | toString: (new Tuple(ident(p), None, False));
			case iterator: (new Tuple(ident(p), None, True));
			case _: (new Tuple(as3(p), None, False));
			};
		case TInst({ cl_path = ([], String) }, _):
			switch (p) {
			case length: (new Tuple(ident(p), None, False));
			case charCodeAt if (Common.defined(ctx.com, Define.NoFlashOverride)): (new Tuple(ident( ^ (p, "HX")), None, True));
			case charCodeAt: (new Tuple(ident(p), None, True));
			case cca: (new Tuple(as3("charCodeAt"), None, False));
			case _: (new Tuple(as3(p), None, False));
			};
		case TInst({ cl_path = ([], Date) }, _):
			switch (p) {
			case toString if (Common.defined(ctx.com, Define.NoFlashOverride)): (new Tuple(ident( ^ (p, "HX")), None, True));
			case _: (new Tuple(ident(p), None, False));
			};
		case TAnon(a): switch (a.a_status.val) {
			case Statics({ cl_path = ([], Math) }):
				switch (p) {
				case POSITIVE_INFINITY | NEGATIVE_INFINITY | NaN: (new Tuple(ident(p), Some(KFloat), False));
				case floor | ceil | round if (ctx.for_call): (new Tuple(ident(p), Some(KInt), False));
				case ffloor | fceil | fround: (new Tuple(ident(String.sub(p, 1, -(String.length(p), 1))), None, False));
				case _: (new Tuple(ident(p), None, False));
				};
			case _: (new Tuple(ident(p), None, False));
			};
		case TInst({ cl_kind = KExtension(_) } = c, params):
			try {
				var f = PMap.find(p, c.cl_fields);
				(new Tuple(ident(p), Some(classify(ctx, apply_params(c.cl_params, params, f.cf_type))), False));
			} catch (e: Not_found) {
				(new Tuple(ident(p), None, False));
			};
		case TInst({ cl_interface = True } = c, _): 	function loop(c) return {
				try {
					switch (PMap.find(p, c.cl_fields)) {
					case {
							cf_kind = Var(_) | Method(MethDynamic)
						}: raise(Exit);
					case _: c;
					};
				} catch (e: Not_found) {
					function loop2(match) return switch (match) {
					case []: raise(Not_found);
					case ::((i, _), l): try {
							loop(i);
						} catch (e: Not_found) {
							loop2(l);
						};
					};
					loop2(c.cl_implements);
				};
			};
			try {
				var c = loop(c);
				var ns = HMName(reserved(p), HNNamespace(switch (c.cl_path) {
			case ([], n): n;
				case (l, n): ^ (String.concat(".", l), ^ (":", n));
				}));
				(new Tuple(ns, None, False));
			} catch (e: Not_found | Exit) {
				(new Tuple(ident(p), None, False));
			};
		case _: (new Tuple(ident(p), None, False));
		};
	};

	public static function default_infos([]) return {
		{
			() with ipos = 0;
			istack = 0;
			imax = 0;
			iregs = DynArray.create([]);
			iscopes = 0;
			imaxscopes = 0;
			iloop = -1;
			icond = False
		};
	};

	public static function alloc_reg(ctx, k) return {
		var regs = ctx.infos.iregs;
		try {
			var p = DynArray.index_of(function r: && (!(r.rused), = (k, r.rtype)), regs);
			var r = DynArray.unsafe_get(regs, p);
			r.rused = True;
			r.rinit = False;
			r;
		} catch (e: Not_found) {
			var r = {
				() with rid = +(DynArray.length(regs), 1);
				rused = True;
				rinit = False;
				rtype = k;
				rcond = False
			};
			DynArray.add(regs, r);
			r;
		};
	};

	public static function coerce(ctx, t) return {
		if (<>(t, KNone)) {
			write(ctx, switch (t) {
		case KInt: HToInt;
		case KUInt: HToUInt;
		case KFloat: HToNumber;
		case KBool: HToBool;
		case KType(t): HCast(t);
			case KDynamic: HAsAny;
			case KNone: assert False;
			});
		} else {
			[];
		};
	};

	public static function set_reg(ctx, r) return {
		if (!(r.rinit)) {
			r.rinit = True;
			if (ctx.infos.icond) {
				r.rcond = True;
			} else {
				[];
			};
		} else {
			[];
		};
		coerce(ctx, r.rtype);
		write(ctx, HSetReg(r.rid));
	};

	public static function set_reg_dup(ctx, r) return {
		if (!(r.rinit)) {
			r.rinit = True;
			if (ctx.infos.icond) {
				r.rcond = True;
			} else {
				[];
			};
		} else {
			[];
		};
		coerce(ctx, r.rtype);
		write(ctx, HDup);
		write(ctx, HSetReg(r.rid));
	};

	public static function free_reg(ctx, r) return {
		r.rused = False;
	};

	public static function pop(ctx, n) return {
		function loop(n) return {
			if ( > (n, 0)) {
				write(ctx, HPop);
				loop(-(n, 1));
			} else {
				[];
			};
		};
		if ( < (n, 0)) {
			assert False;
		} else {
			[];
		};
		var old = ctx.infos.istack;
		loop(n);
		ctx.infos.istack = old;
	};

	public static function is_member(ctx, name) return {
		function loop(c) return {
			|| (PMap.mem(name, c.cl_fields), switch (c.cl_super) {
		case None: False;
		case Some(c, _): loop(c);
			});
		};
		loop(ctx.cur_class);
	};

	public static function rename_block_var(ctx, v) return {
		function loop(i) return {
			var name = ^ (v.v_name, string_of_int(i));
			if ( || (List.exists(function (_, x, _): = (name, x), ctx.block_vars), is_member(ctx, name))) {
				loop(+(i, 1));
			} else {
				v.v_name = name;
			};
		};
		loop(1);
	};

	public static function define_local(ctx, ? : (init = False), v, p) return {
		var name = v.v_name;
		var t = v.v_type;
		var l = if (v.v_capture) {
			var topt = type_opt(ctx, t);
			if ( || (List.exists(function (_, x, _): = (name, x), ctx.block_vars), is_member(ctx, name))) {
				rename_block_var(ctx, v);
			} else {
				[];
			};
			var pos = +(List.length(ctx.block_vars), 1);
			ctx.block_vars = ::((new Tuple(pos, v.v_name, topt)), ctx.block_vars);
			LScope(pos);
		} else {
			var r = alloc_reg(ctx, classify(ctx, t));
			if (ctx.debug) {
				write(ctx, HDebugReg(name, r.rid, ctx.last_line));
			} else {
				[];
			};
			r.rinit = init;
			LReg(r);
		};
		ctx.locals = PMap.add(v.v_id, (new Tuple(v, l)), ctx.locals);
	};

	public static function is_set(v) return {
		= (Obj.magic(v), Write);
	};

	public static function gen_local_access(ctx, v, p, forseta) return {
		switch (snd(try {
			PMap.find(v.v_id, ctx.locals);
			} catch (e: Not_found) {
				error( ^ ("Unbound variable ", v.v_name), p);
			})) {
		case LReg(r): VReg(r);
		case LScope(n): write(ctx, HGetScope(1));
			VScope(n);
		case LGlobal(p): if (is_set(forset)) {
				write(ctx, HFindProp(p));
			} else {
				[];
			};
			VGlobal(p);
		} : access(a);
	};

	public static function get_local_register(ctx, v) return {
		switch (try {
			snd(PMap.find(v.v_id, ctx.locals));
			} catch (e: Not_found) {
				LScope(0);
			}) {
		case LReg(r): Some(r);
		case _: None;
		};
	};

	public static function setvar(ctx, accaccess(write), kret) return {
		switch (acc) {
		case VReg(r): if (<>(kret, None)) {
				set_reg_dup(ctx, r);
			} else {
				set_reg(ctx, r);
			};
		case VGlobal(_) | VId(_) | VCast(_) | VArray | VScope(_) if (<>(kret, None)): var r = alloc_reg(ctx, switch (kret) {
		case None: assert False;
		case Some(k): k;
			});
			set_reg_dup(ctx, r);
			setvar(ctx, acc, None);
			write(ctx, HReg(r.rid));
			free_reg(ctx, r);
		case VGlobal(g): write(ctx, HSetProp(g));
		case VId(id) | VCast(id, _): write(ctx, HInitProp(id));
		case VVolatile(id, _): write(ctx, HArray(1));
			write(ctx, HInitProp(id));
		case VArray: write(ctx, HSetProp(dynamic_prop));
			ctx.infos.istack = -(ctx.infos.istack, 1);
		case VScope(n): write(ctx, HSetSlot(n));
		case VSuper(id): write(ctx, HSetSuper(id));
		};
	};

	public static function getvar(ctx, accaccess(read)) return {
		switch (acc) {
		case VReg(r): if (!(r.rinit)) {
				r.rinit = True;
				r.rcond = True;
			} else {
				[];
			};
			write(ctx, HReg(r.rid));
		case VId(id): write(ctx, HGetProp(id));
		case VVolatile(id, t): write(ctx, HGetProp(id));
			write(ctx, HSmallInt(0));
			write(ctx, HGetProp(dynamic_prop));
			ctx.infos.istack = -(ctx.infos.istack, 1);
			switch (t) {
			case None: [];
			case Some(t): coerce(ctx, t);
			};
		case VCast(id, t): write(ctx, HGetProp(id));
			coerce(ctx, t);
		case VGlobal(g): write(ctx, HGetLex(g));
		case VArray: write(ctx, HGetProp(dynamic_prop));
			ctx.infos.istack = -(ctx.infos.istack, 1);
		case VScope(n): write(ctx, HGetSlot(n));
		case VSuper(id): write(ctx, HGetSuper(id));
		};
	};

	public static function open_block(ctx, retval) return {
		var old_stack = ctx.infos.istack;
		var old_regs = DynArray.map(function r: r.rused, ctx.infos.iregs);
		var old_locals = ctx.locals;
		function []:
		if (<>(ctx.infos.istack, +(old_stack, if (retval) {
			1;
		} else {
			0;
		}))) {
			assert False;
		} else {
			[];
		};
		var rcount = +(DynArray.length(old_regs), 1);
		DynArray.iter(function r:
		if ( < (r.rid, rcount)) {
		r.rused = DynArray.unsafe_get(old_regs, -(r.rid, 1));
		} else {
			r.rused = False;
		}, ctx.infos.iregs);
		ctx.locals = old_locals;
	};

	public static function begin_branch(ctx) return {
		if (ctx.infos.icond) {
			function []: [];
		} else {
			ctx.infos.icond = True;
			function []: ctx.infos.icond = False;
		};
	};

	public static function begin_switch(ctx) return {
		var branch = begin_branch(ctx);
		var switch_index = DynArray.length(ctx.code);
		var switch_pos = ctx.infos.ipos;
		write(ctx, HSwitch(0, []));
		var constructs = ref([]);
		var max = ref(0);
		function ftag(tag) return {
			if ( > (tag, max.val)) {
				max.val = tag;
			} else {
				[];
			};
			constructs.val = ::((new Tuple(tag, ctx.infos.ipos)), constructs.val);
		};
		function fend([]) return {
			var cases = Array.create(+(max.val, 1), 1);
			List.iter(function (tag, pos): Array.set(cases, tag, -(pos, switch_pos)), constructs.val);
			DynArray.set(ctx.code, switch_index, HSwitch(1, Array.to_list(cases)));
			branch([]);
		};
		(new Tuple(fend, ftag));
	};

	public static function debug_infos( ? : (is_min = True), ctx, p) return {
		if (ctx.debug) {
			var line = Lexer.get_error_line(if (is_min) {
			p;
		} else {
			{
				(p) with pmin = p.pmax
				};
			});
			if (<>(ctx.last_file, p.pfile)) {
				write(ctx, HDebugFile(if (ctx.debugger) {
				Common.get_full_path(p.pfile);
				} else {
					p.pfile;
				}));
				ctx.last_file = p.pfile;
				ctx.last_line = -1;
			} else {
				[];
			};
			if (<>(ctx.last_line, line)) {
				write(ctx, HDebugLine(line));
				ctx.last_line = line;
			} else {
				[];
			};
		} else {
			[];
		};
	};

	public static function to_utf8(str) return {
		try {
			UTF8.validate(str);
			str;
		} catch (e: UTF8.Malformed_code) {
			var b = UTF8.Buf.create(0);
			String.iter(function c: UTF8.Buf.add_char(b, UChar.of_char(c)), str);
			UTF8.Buf.contents(b);
		};
	};

	public static function gen_constant(ctx, c, t, p) return {
		switch (c) {
		case TInt(i): var unsigned = = (classify(ctx, t), KUInt);
			if ( && ( > (Int32.compare(i, ), 0), < (Int32.compare(i, ), 0))) {
				write(ctx, HSmallInt(Int32.to_int(i)));
				if (unsigned) {
					write(ctx, HToUInt);
				} else {
					[];
				};
			} else {
				write(ctx, if (unsigned) {
				HUIntRef(i);
				} else {
					HIntRef(i);
				});
			};
		case TFloat(f): var f = float_of_string(f);
			write(ctx, HFloat(f));
		case TString(s): write(ctx, HString(to_utf8(s)));
		case TBool(b): write(ctx, if (b) {
			HTrue;
		} else {
			HFalse;
		});
		case TNull: write(ctx, HNull);
			coerce(ctx, classify(ctx, t));
		case TThis: write(ctx, HThis);
		case TSuper: assert False;
		};
	};

	public static function end_fun(ctx, args, dparams, tret) return {
		{
			() with hlmt_index = 0;
			hlmt_ret = type_void(ctx, tret);
			hlmt_args = List.map(function (v, _): type_opt(ctx, v.v_type), args);
			hlmt_native = False;
			hlmt_var_args = False;
			hlmt_debug_name = None;
			hlmt_dparams = dparams;
			hlmt_pnames = if ( || (ctx.swc, ctx.debugger)) {
				Some(List.map(function (v, _): Some(v.v_name), args));
			} else {
				None;
			};
			hlmt_new_block = False;
			hlmt_unused_flag = False;
			hlmt_arguments_defined = False;
			hlmt_uses_dxns = False;
			hlmt_function = None
		};
	};

	public static function begin_fun(ctx, args, tret, el, stat, p) return {
		var old_locals = ctx.locals;
		var old_code = ctx.code;
		var old_infos = ctx.infos;
		var old_trys = ctx.trys;
		var old_bvars = ctx.block_vars;
		var old_static = ctx.in_static;
		var last_line = ctx.last_line;
		var old_treg = ctx.try_scope_reg;
		ctx.infos = default_infos([]);
		ctx.code = DynArray.create([]);
		ctx.trys = [];
		ctx.block_vars = [];
		ctx.in_static = stat;
		ctx.last_line = -1;
		ctx.last_file = "";
		debug_infos(ctx, p);
		function find_this(e) return {
			switch (e.eexpr) {
			case TFunction(_): [];
			case TConst(TThis) | TConst(TSuper): raise(Exit);
			case _: Type.iter(find_this, e);
			};
		};
		var this_reg = try {
			List.iter(find_this, el);
			False;
		} catch (e: Exit) {
			True;
		};
		ctx.locals = PMap.foldi(function _: function (v, l): function acc:
		switch (l) {
	case LReg(_): acc;
		case LScope(_): PMap.add(v.v_id, (new Tuple(v, LGlobal(ident(v.v_name)))), acc);
		case LGlobal(_): PMap.add(v.v_id, (new Tuple(v, l)), acc);
		}, ctx.locals, PMap.empty);
		var dparams = ref(None);
		function make_constant_value(r, c, t) return {
			var v = switch ((new Tuple(classify(ctx, t), c))) {
			case (_, None): HVNone;
			case (KInt | KFloat | KUInt | KBool = kind, Some(c)): switch (c) {
				case TInt(i): if ( = (kind, KUInt)) {
						HVUInt(i);
					} else {
						HVInt(i);
					};
				case TFloat(s): HVFloat(float_of_string(s));
				case TBool(b): HVBool(b);
				case TNull: error( ^ ("In Flash9, null can't be used as basic type ", s_type(print_context([]), t)), p);
				case _: assert False;
				};
			case (_, Some(TNull)): HVNone;
			case (k, Some(c)): write(ctx, HReg(r.rid));
				write(ctx, HNull);
				var j = jump(ctx, J3Neq);
				gen_constant(ctx, c, t, p);
				coerce(ctx, k);
				write(ctx, HSetReg(r.rid));
				j([]);
				HVNone;
			};
			switch (dparams.val) {
			case None: if (<>(c, None)) {
					dparams.val = Some(::(v, []));
				} else {
					[];
				};
			case Some(l): dparams.val = Some(::(v, l));
			};
		};
		var Tuple(args, varargs) = switch (List.rev(args)) {
		case ::(( {
							  v_name = __arguments__;
							  v_type = t
						  } = v, _), l):
			switch (follow(t)) {
			case TInst({ cl_path = ([], Array) }, _): (new Tuple(List.rev(l), Some(v, True)));
			case _: (new Tuple(List.rev(l), Some(v, False)));
			};
		case _: (new Tuple(args, None));
		};
		List.iter(function (v, c): var t = v.v_type;
				  define_local(ctx, v, init = True, p);
		switch (gen_local_access(ctx, v, null_pos, Write)) {
	case VReg(r): make_constant_value(r, c, t);
		case acc: var r = alloc_reg(ctx, classify(ctx, t));
			make_constant_value(r, c, t);
			write(ctx, HReg(r.rid));
			setvar(ctx, acc, None);
		}, args);
		switch (varargs) {
		case None: [];
		case Some(v, _): define_local(ctx, v, init = True, p);
			ignore(alloc_reg(ctx, classify(ctx, v.v_type)));
		};
		var dparams = switch (dparams.val) {
		case None: None;
		case Some(l): Some(List.rev(l));
		};
		function is_not_rethrow(Tuple(_, e)) return {
			switch (e.eexpr) {
			case TBlock(::({ eexpr = TThrow({ eexpr = TNew(_, _, []) }) }, [])): False;
			case _: True;
			};
		};
		function loop_try(e) return {
			switch (e.eexpr) {
			case TFunction(_): [];
			case TTry(_, catches) if (List.exists(is_not_rethrow, catches)): raise(Exit);
			case _: Type.iter(loop_try, e);
			};
		};
		ctx.try_scope_reg = try {
			List.iter(loop_try, el);
			None;
		} catch (e: Exit) {
			Some(alloc_reg(ctx, KDynamic));
		};
		function []: var hasblock = || (<>(ctx.block_vars, []), <>(ctx.try_scope_reg, None));
		var code = DynArray.to_list(ctx.code);
		var extra = if (hasblock) {
			var scope = switch (ctx.try_scope_reg) {
			case None: ::(HScope, []);
			case Some(r): ::(HDup, ::(HSetReg(r.rid), ::(HScope, [])));
			};
			::(HThis, ::(HScope, ::(HNewBlock, scope)));
		} else {
			if (this_reg) {
				::(HThis, ::(HScope, []));
			} else {
				[];
			};
		};
		var extra = @(extra, List.concat(List.map(function r:
		if (!(r.rcond)) {
		[];
		} else {
			var s = ::(HSetReg(r.rid), []);
			switch (r.rtype) {
			case KInt: ::(HSmallInt(0), s);
			case KUInt: ::(HSmallInt(0), ::(HToUInt, s));
			case KFloat: ::(HNaN, s);
			case KBool: ::(HFalse, s);
			case KType(t): ::(HNull, ::(HAsType(t), s));
			case KDynamic: ::(HNull, ::(HAsAny, s));
			case KNone: ::(HNull, ::(HAsType(HMPath([], "Class")), s));
			};
		}, DynArray.to_list(ctx.infos.iregs))));
		var delta = List.length(extra);
		var f = { () with hlf_stack_size = if ( && ( = (ctx.infos.imax, 0), || (hasblock, this_reg))) {
		1;
	} else {
		ctx.infos.imax;
	};
	hlf_nregs = +(DynArray.length(ctx.infos.iregs), 1);
				hlf_init_scope = 1;
	hlf_max_scope = +(+(ctx.infos.imaxscopes, 1), if (hasblock) {
		2;
	} else {
		if (this_reg) {
				1;
			} else {
				0;
			};
		});
		hlf_code = MultiArray.of_array(Array.of_list(@(extra, code)));
				   hlf_trys = Array.of_list(List.map(function t: { () with hltc_start = +(t.tr_pos, delta);
											hltc_end = +(t.tr_end, delta);
											hltc_handle = +(t.tr_catch_pos, delta);
											hltc_type = type_opt(ctx, t.tr_type);
											hltc_name = None
																 }, List.rev(ctx.trys)));
				   hlf_locals = Array.of_list(List.map(function (id, name, t): (new Tuple(ident(name), t, id, False)), ctx.block_vars))
				};
		var mt = { (end_fun(ctx, args, dparams, tret)) with hlmt_var_args = switch (varargs) {
	case Some(_, True): True;
		case _: False;
		};
		hlmt_arguments_defined = switch (varargs) {
	case Some(_, False): True;
		case _: False;
		};
		hlmt_new_block = hasblock;
						 hlmt_function = Some(f)
				 };
		ctx.locals = old_locals;
		ctx.code = old_code;
		ctx.infos = old_infos;
		ctx.trys = old_trys;
		ctx.block_vars = old_bvars;
		ctx.in_static = old_static;
		ctx.last_line = last_line;
		ctx.try_scope_reg = old_treg;
		mt;
	};

	public static function empty_method(ctx, p) return {
		var f = begin_fun(ctx, [], ctx.com.basic.tvoid, [], True, p);
		write(ctx, HRetVoid);
		f([]);
	};

	public static function begin_loop(ctx) return {
		var old_loop = ctx.infos.iloop;
		var old_breaks = ctx.breaks;
		var old_conts = ctx.continues;
		ctx.infos.iloop = ctx.infos.istack;
		ctx.breaks = [];
		ctx.continues = [];
		function cont_pos:
		if (<>(ctx.infos.istack, ctx.infos.iloop)) {
			assert False;
		} else {
			[];
		};
		List.iter(function j: j([]), ctx.breaks);
		List.iter(function j: j(cont_pos), ctx.continues);
		ctx.infos.iloop = old_loop;
		ctx.breaks = old_breaks;
		ctx.continues = old_conts;
	};

	public static function no_value(ctx, retval) return {
		if (retval) {
			ctx.infos.istack = +(ctx.infos.istack, 1);
		} else {
			[];
		};
	};

	public static function pop_value(ctx, retval) return {
		if (retval) {
			ctx.infos.istack = -(ctx.infos.istack, 1);
		} else {
			[];
		};
	};

	public static var gen_expr_ref = ref(function _: function _: function _: assert False);

	public static function gen_expr(ctx, e, retval) return {
		gen_expr_ref.val(ctx, e, retval);
	};

	public static function gen_access(ctx, e, forseta) return {
		switch (e.eexpr) {
		case TLocal(v): gen_local_access(ctx, v, e.epos, forset);
		case TField({ eexpr = TConst(TSuper) } = e1, f): var f = field_name(f);
			var Tuple(id, _, _) = property(ctx, f, e1.etype);
			write(ctx, HThis);
			VSuper(id);
		case TEnumParameter(e1, _, i): gen_expr(ctx, True, e1);
			write(ctx, HGetProp(ident("params")));
			write(ctx, HSmallInt(i));
			VArray;
		case TField(e1, fa): var f = field_name(fa);
			var Tuple(id, k, closure) = property(ctx, f, e1.etype);
			if ( && (closure, !(ctx.for_call))) {
				error("In Flash9, this method cannot be accessed this way : please define a local function", e1.epos);
			} else {
				[];
			};
			switch (e1.eexpr) {
			case TConst(TThis | TSuper) if (!(ctx.in_static)): write(ctx, HFindProp(id));
			case _: gen_expr(ctx, True, e1);
			};
			switch (k) {
			case Some(t): VCast(id, t);
			case None: switch ((new Tuple(follow(e1.etype), follow(e.etype)))) {
				case (_, TFun(_)) if (!(ctx.for_call)): VCast(id, classify(ctx, e.etype));
				case (TEnum(_), _): VId(id);
				case (TInst(_, tl), et): var is_type_parameter_field = switch (fa) {
					case FInstance(_, _, cf): switch (follow(cf.cf_type)) {
						case TInst({ cl_kind = KTypeParameter(_) }, _): True;
						case _: False;
						};
					case _: List.exists(function t: == (follow(t), et), tl);
					};
					if (is_type_parameter_field) {
						VCast(id, classify(ctx, e.etype));
					} else {
						if (Codegen.is_volatile(e.etype)) {
							VVolatile(id, None);
						} else {
							VId(id);
						};
					};
				case (TAnon(a), _) if (switch (a.a_status.val) {
						case Statics(_) | EnumStatics(_): True;
							case _: False;
							}):
					if (Codegen.is_volatile(e.etype)) {
						VVolatile(id, None);
					} else {
						VId(id);
					};
				case _: if (Codegen.is_volatile(e.etype)) {
						VVolatile(id, Some(classify(ctx, e.etype)));
					} else {
						VCast(id, classify(ctx, e.etype));
					};
				};
			};
		case TArray({ eexpr = TLocal({ v_name = __global__ }) }, { eexpr = TConst(TString(s)) }): var path = parse_path(s);
			var id = type_path(ctx, path);
			if (is_set(forset)) {
				write(ctx, HGetGlobalScope);
			} else {
				[];
			};
			VGlobal(id);
		case TArray(e, eindex): gen_expr(ctx, True, e);
			gen_expr(ctx, True, eindex);
			VArray;
		case TTypeExpr(t): var id = type_path(ctx, t_path(t));
			if (is_set(forset)) {
				write(ctx, HGetGlobalScope);
			} else {
				[];
			};
			VGlobal(id);
		case _: invalid_expr(e.epos);
		} : access(a);
	};

	public static function gen_expr_twice(ctx, e) return {
		switch (e.eexpr) {
		case TLocal(v): switch (get_local_register(ctx, v)) {
			case Some(r): write(ctx, HReg(r.rid));
				write(ctx, HReg(r.rid));
			case None: gen_expr(ctx, True, e);
				write(ctx, HDup);
			};
		case TConst(_): gen_expr(ctx, True, e);
			gen_expr(ctx, True, e);
		case _: gen_expr(ctx, True, e);
			write(ctx, HDup);
		};
	};

	public static function gen_access_rw(ctx, e) return {
		switch (e.eexpr) {
		case TArray({ eexpr = TLocal(_) }, { eexpr = TConst(_) }) | TArray({ eexpr = TLocal(_) }, { eexpr = TLocal(_) }) | TField({ eexpr = TLocal(_) }, _) | TField({ eexpr = TConst(_) }, _)
			: var w = gen_access(ctx, e, Write);
			var r = gen_access(ctx, e, Read);
			(new Tuple(r, w));
		case TArray(e, eindex): var r = switch (e.eexpr) {
			case TLocal(v): get_local_register(ctx, v);
			case _: None;
			};
			switch (r) {
			case None: var r = alloc_reg(ctx, classify(ctx, e.etype));
				gen_expr(ctx, True, e);
				set_reg(ctx, r);
				write(ctx, HReg(r.rid));
				gen_expr_twice(ctx, eindex);
				write(ctx, HReg(r.rid));
				write(ctx, HSwap);
				free_reg(ctx, r);
			case Some(r): write(ctx, HReg(r.rid));
				gen_expr_twice(ctx, eindex);
				write(ctx, HReg(r.rid));
				write(ctx, HSwap);
			};
			(new Tuple(VArray, VArray));
		case TField(_): var w = gen_access(ctx, e, Write);
			write(ctx, HDup);
			(new Tuple(Obj.magic(w), w));
		case _: var w = gen_access(ctx, e, Write);
			var r = gen_access(ctx, e, Read);
			(new Tuple(r, w));
		} : Tuple<access(read) * access(write)>;
	};

	public static function gen_type(ctx, t) return {
		switch (t) {
		case HMParams(t, tl): write(ctx, HGetLex(t));
			List.iter(gen_type(ctx), tl);
			write(ctx, HApplyType(List.length(tl)));
		case _: write(ctx, HGetLex(t));
		};
	};

	public static function gen_expr_content(ctx, retval, e) return {
		switch (e.eexpr) {
		case TConst(c): gen_constant(ctx, c, e.etype, e.epos);
		case TThrow(e): ctx.infos.icond = True;
			if (has_feature(ctx.com, "haxe.CallStack.exceptionStack")) {
				getvar(ctx, VGlobal(type_path(ctx, (new Tuple(::("flash", []), "Boot")))));
				var id = type_path(ctx, (new Tuple(::("flash", ::("errors", [])), "Error")));
				write(ctx, HFindPropStrict(id));
				write(ctx, HConstructProperty(id, 0));
				setvar(ctx, VId(ident("lastError")), None);
			} else {
				[];
			};
			gen_expr(ctx, True, e);
			write(ctx, HThrow);
			no_value(ctx, retval);
		case TParenthesis(e) | TMeta(_, e): gen_expr(ctx, retval, e);
		case TObjectDecl(fl): List.iter(function (name, e): write(ctx, HString(reserved(name)));
											gen_expr(ctx, True, e), fl);
			write(ctx, HObject(List.length(fl)));
		case TArrayDecl(el): List.iter(gen_expr(ctx, True), el);
			write(ctx, HArray(List.length(el)));
		case TBlock(el): function loop(match) return switch (match) {
			case []: if (retval) {
					write(ctx, HNull);
				} else {
					[];
				};
			case ::(e, []): gen_expr(ctx, retval, e);
			case ::(e, l): gen_expr(ctx, False, e);
				loop(l);
			};
			var b = open_block(ctx, retval);
			loop(el);
			b([]);
		case TVar(v, ei): define_local(ctx, v, e.epos);
			switch (ei) {
			case None: [];
			case Some(e): var acc = gen_local_access(ctx, v, e.epos, Write);
				gen_expr(ctx, True, e);
				setvar(ctx, acc, None);
			};
		case TReturn(None): write(ctx, HRetVoid);
			ctx.infos.icond = True;
			no_value(ctx, retval);
		case TReturn(Some(e)): gen_expr(ctx, True, e);
			write(ctx, HRet);
			ctx.infos.icond = True;
			no_value(ctx, retval);
		case TField(_) | TLocal(_) | TTypeExpr(_): getvar(ctx, gen_access(ctx, e, Read));
		case TEnumParameter(_) | TArray(_): getvar(ctx, gen_access(ctx, e, Read));
			coerce(ctx, classify(ctx, e.etype));
		case TBinop(op, e1, e2): gen_binop(ctx, retval, op, e1, e2, e.etype, e.epos);
		case TCall(f, el): gen_call(ctx, retval, f, el, e.etype);
		case TNew({ cl_path = ([], Array) }, _, []): write(ctx, HArray(0));
		case TNew(c, tl, pl): var id = type_id(ctx, TInst(c, tl));
			switch (id) {
			case HMParams(_): gen_type(ctx, id);
				List.iter(gen_expr(ctx, True), pl);
				write(ctx, HConstruct(List.length(pl)));
			case _: write(ctx, HFindPropStrict(id));
				List.iter(gen_expr(ctx, True), pl);
				write(ctx, HConstructProperty(id, List.length(pl)));
			};
		case TFunction(f): write(ctx, HFunction(generate_function(ctx, f, True)));
		case TIf(e0, e1, e2): var j = jump_expr(ctx, e0, False);
			var branch = begin_branch(ctx);
			gen_expr(ctx, retval, e1);
			var t = classify(ctx, e.etype);
			if ( && (retval, <>(classify(ctx, e1.etype), t))) {
				coerce(ctx, t);
			} else {
				[];
			};
			switch (e2) {
			case None: j([]);
			case Some(e): pop_value(ctx, retval);
				var jend = jump(ctx, J3Always);
				j([]);
				gen_expr(ctx, retval, e);
				if ( && (retval, <>(classify(ctx, e.etype), t))) {
					coerce(ctx, t);
				} else {
					[];
				};
				jend([]);
			};
			branch([]);
		case TWhile(econd, e, flag): var jstart = jump(ctx, J3Always);
			var end_loop = begin_loop(ctx);
			var branch = begin_branch(ctx);
			var loop = jump_back(ctx);
			if ( = (flag, DoWhile)) {
				jstart([]);
			} else {
				[];
			};
			gen_expr(ctx, False, e);
			if ( = (flag, NormalWhile)) {
				jstart([]);
			} else {
				[];
			};
			var continue_pos = ctx.infos.ipos;
			var _ = jump_expr_gen(ctx, econd, True, function j: loop(j);
								  function []: []);
			branch([]);
			end_loop(continue_pos);
			if (retval) {
				write(ctx, HNull);
			} else {
				[];
			};
		case TUnop(op, flag, e): gen_unop(ctx, retval, op, flag, e);
		case TTry(e2, cases): if (<>(ctx.infos.istack, 0)) {
				error("Cannot compile try/catch as a right-side expression in Flash9", e.epos);
			} else {
				[];
			};
			var branch = begin_branch(ctx);
			var p = ctx.infos.ipos;
			gen_expr(ctx, retval, e2);
			var pend = ctx.infos.ipos;
			var jend = jump(ctx, J3Always);
			function loop(ncases) return {
			case []: [];
			case ::((v, e), l): var b = open_block(ctx, retval);
				var t = v.v_type;
				ctx.trys = ::({
					() with tr_pos = p;
					tr_end = pend;
					tr_catch_pos = ctx.infos.ipos;
					tr_type = t
				}, ctx.trys);
				ctx.infos.istack = +(ctx.infos.istack, 1);
				if ( < (ctx.infos.imax, ctx.infos.istack)) {
					ctx.infos.imax = ctx.infos.istack;
				} else {
					[];
				};
				write(ctx, HThis);
				write(ctx, HScope);
				switch (ctx.try_scope_reg) {
				case None: [];
				case Some(r): write(ctx, HReg(r.rid));
					write(ctx, HScope);
				};
				define_local(ctx, v, e.epos);
				var r = switch (snd(try {
					PMap.find(v.v_id, ctx.locals);
					} catch (e: Not_found) {
						assert False;
					})) {
				case LReg(_): None;
				case _: var r = alloc_reg(ctx, classify(ctx, t));
					set_reg(ctx, r);
					Some(r);
				};
				var acc = gen_local_access(ctx, v, e.epos, Write);
				switch (r) {
				case None: [];
				case Some(r): write(ctx, HReg(r.rid));
				};
				setvar(ctx, acc, None);
				function call_loop(e) return {
					switch (e.eexpr) {
					case TCall(_) | TNew(_): raise(Exit);
					case TFunction(_): [];
					case _: Type.iter(call_loop, e);
					};
				};
				var has_call = try {
					call_loop(e);
					False;
				} catch (e: Exit) {
					True;
				};
				if ( && (has_call, has_feature(ctx.com, "haxe.CallStack.exceptionStack"))) {
					getvar(ctx, gen_local_access(ctx, v, e.epos, Read));
					write(ctx, HAsType(type_path(ctx, (new Tuple(::("flash", ::("errors", [])), "Error")))));
					var j = jump(ctx, J3False);
					getvar(ctx, VGlobal(type_path(ctx, (new Tuple(::("flash", []), "Boot")))));
					getvar(ctx, gen_local_access(ctx, v, e.epos, Read));
					setvar(ctx, VId(ident("lastError")), None);
					j([]);
				} else {
					[];
				};
				gen_expr(ctx, retval, e);
				b([]);
				if (retval) {
					ctx.infos.istack = -(ctx.infos.istack, 1);
				} else {
					[];
				};
				switch (l) {
				case []: [];
				case _: var j = jump(ctx, J3Always);
					::(j, loop(+(ncases, 1), l));
				};
			};
			var loops = loop(List.length(ctx.trys), cases);
			List.iter(function j: j([]), loops);
			branch([]);
			jend([]);
		case TFor(v, it, e): gen_expr(ctx, True, it);
			var r = alloc_reg(ctx, KDynamic);
			set_reg(ctx, r);
			var branch = begin_branch(ctx);
			var b = open_block(ctx, retval);
			define_local(ctx, v, e.epos);
			var end_loop = begin_loop(ctx);
			var continue_pos = ctx.infos.ipos;
			var start = jump_back(ctx);
			write(ctx, HReg(r.rid));
			write(ctx, HCallProperty(ident("hasNext"), 0));
			var jend = jump(ctx, J3False);
			var acc = gen_local_access(ctx, v, e.epos, Write);
			write(ctx, HReg(r.rid));
			write(ctx, HCallProperty(ident("next"), 0));
			setvar(ctx, acc, None);
			gen_expr(ctx, False, e);
			start(J3Always);
			end_loop(continue_pos);
			jend([]);
			if (retval) {
				getvar(ctx, gen_local_access(ctx, v, e.epos, Read));
			} else {
				[];
			};
			b([]);
			branch([]);
			free_reg(ctx, r);
		case TBreak: pop(ctx, -(ctx.infos.istack, ctx.infos.iloop));
			ctx.breaks = ::(jump(ctx, J3Always), ctx.breaks);
			no_value(ctx, retval);
		case TContinue: pop(ctx, -(ctx.infos.istack, ctx.infos.iloop));
			var op = DynArray.length(ctx.code);
			var p = ctx.infos.ipos;
			write(ctx, HJump(J3Always, 0));
			ctx.continues = ::(function target: DynArray.set(ctx.code, op, HJump(J3Always, -(target, p))), ctx.continues);
			no_value(ctx, retval);
		case TSwitch(e0, el, eo): var t = classify(ctx, e.etype);
			try {
				var t0 = classify(ctx, e0.etype);
				if ( && (<>(t0, KInt), <>(t0, KUInt))) {
					raise(Exit);
				} else {
					[];
				};
				function get_int(e) return {
					switch (e.eexpr) {
					case TConst(TInt(n)): if ( || (<(n, ), >(n, ))) {
							raise(Exit);
						} else {
							[];
						};
						Int32.to_int(n);
					case TParenthesis(e) | TBlock(::(e, [])) | TMeta(_, e): get_int(e);
					case _: raise(Not_found);
					};
				};
				List.iter(function (vl, _): List.iter(function v:
				try {
					ignore(get_int(v));
				} catch (e: _) {
					raise(Exit);
				}, vl), el);
				gen_expr(ctx, True, e0);
				if (<>(t0, KInt)) {
					write(ctx, HToInt);
				} else {
					[];
				};
				var Tuple(switch, case) = begin_switch(ctx);
				switch (eo) {
				case None: if (retval) {
						write(ctx, HNull);
						coerce(ctx, t);
					} else {
						[];
					};
				case Some(e): gen_expr(ctx, retval, e);
					if ( && (retval, <>(classify(ctx, e.etype), t))) {
						coerce(ctx, t);
					} else {
						[];
					};
				};
				var jends = List.map(function (vl, e): var j = jump(ctx, J3Always);
									 List.iter(function v:
										   case (get_int(v)), vl);
										 pop_value(ctx, retval);
										 gen_expr(ctx, retval, e);
					if ( && (retval, <>(classify(ctx, e.etype), t))) {
					coerce(ctx, t);
					} else {
						[];
					};
				j, el);
				List.iter(function j: j([]), jends);
				switch ([]);
			} catch (e: Exit) {
				var r = alloc_reg(ctx, classify(ctx, e0.etype));
				gen_expr(ctx, True, e0);
				set_reg(ctx, r);
				var branch = begin_branch(ctx);
				var prev = ref(function []: []);
				var jend = List.map(function (vl, e): prev.val([]);
				function loop(match) return switch (match) {
			case []: assert False;
				case ::(v, []): write(ctx, HReg(r.rid));
					gen_expr(ctx, True, v);
					prev.val = jump(ctx, J3Neq);
				case ::(v, l): write(ctx, HReg(r.rid));
					gen_expr(ctx, True, v);
					var j = jump(ctx, J3Eq);
					loop(l);
					j([]);
				};
				loop(vl);
				gen_expr(ctx, retval, e);
				pop_value(ctx, retval);
				if ( && (retval, <>(classify(ctx, e.etype), t))) {
				coerce(ctx, t);
				} else {
					[];
				};
				jump(ctx, J3Always), el);
				prev.val([]);
				free_reg(ctx, r);
				switch (eo) {
				case None: if (retval) {
						write(ctx, HNull);
						coerce(ctx, t);
					} else {
						[];
					};
				case Some(e): gen_expr(ctx, retval, e);
					if ( && (retval, <>(classify(ctx, e.etype), t))) {
						coerce(ctx, t);
					} else {
						[];
					};
				};
				List.iter(function j: j([]), jend);
				branch([]);
			};
		case TCast(e1, t): gen_expr(ctx, retval, e1);
			if (retval) {
				switch (t) {
				case None: var t1 = classify(ctx, e1.etype);
					var t = classify(ctx, e.etype);
					if (<>(t1, t)) {
						coerce(ctx, t);
					} else {
						[];
					};
				case Some(t): var tid = switch (gen_access(ctx, mk(TTypeExpr(t), t_dynamic, e.epos), Read)) {
					case VGlobal(id): id;
					case _: assert False;
					};
					switch (classify(ctx, e.etype)) {
					case KType(n) if (switch (n) {
							case HMPath([], String): False;
								case _: True;
								}): write(ctx, HCast(tid));
					case KInt | KUInt: write(ctx, HDup);
						write(ctx, HIsType(HMPath([], "Number")));
						var j = jump(ctx, J3True);
						write(ctx, HString("Class cast error"));
						write(ctx, HThrow);
						j([]);
						write(ctx, HCast(tid));
					case _: write(ctx, HDup);
						write(ctx, HIsType(tid));
						var j = jump(ctx, J3True);
						write(ctx, HString("Class cast error"));
						write(ctx, HThrow);
						j([]);
						write(ctx, HCast(tid));
					};
				};
			} else {
				[];
			};
		};
	};

	public static function gen_call(ctx, retval, e, el, r) return {
		switch ((new Tuple(e.eexpr, el))) {
		case (TLocal({ v_name = __is__ }), ::(e, ::(t, []))): gen_expr(ctx, True, e);
			gen_expr(ctx, True, t);
			write(ctx, HOp(A3OIs));
		case (TField(_, FStatic({ cl_path = ([], Std) }, { cf_name = is })), ::(e, ::({ eexpr = TTypeExpr(TClassDecl(_)) } = t, [])))
				: gen_expr(ctx, True, e);
			gen_expr(ctx, True, t);
			write(ctx, HOp(A3OIs));
		case (TLocal({ v_name = __as__ }), ::(e, ::(t, []))): gen_expr(ctx, True, e);
			gen_expr(ctx, True, t);
			write(ctx, HOp(A3OAs));
		case (TLocal({ v_name = __int__ }), ::(e, [])): gen_expr(ctx, True, e);
			write(ctx, HToInt);
		case (TLocal({ v_name = __float__ }), ::(e, [])): gen_expr(ctx, True, e);
			write(ctx, HToNumber);
		case (TLocal({ v_name = __foreach__ }), ::(obj, ::(counter, []))): gen_expr(ctx, True, obj);
			gen_expr(ctx, True, counter);
			write(ctx, HForEach);
		case (TLocal({ v_name = __forin__ }), ::(obj, ::(counter, []))): gen_expr(ctx, True, obj);
			gen_expr(ctx, True, counter);
			write(ctx, HForIn);
		case (TLocal({ v_name = __has_next__ }), ::(obj, ::(counter, []))): var oreg = switch (gen_access(ctx, obj, Read)) {
			case VReg(r): r;
			case _: error("Must be a local variable", obj.epos);
			};
			var creg = switch (gen_access(ctx, counter, Read)) {
			case VReg(r): r;
			case _: error("Must be a local variable", obj.epos);
			};
			write(ctx, HNext(oreg.rid, creg.rid));
		case (TLocal({ v_name = __hkeys__ }), ::(e2, [])) | (TLocal({ v_name = __foreach__ }), ::(e2, [])) | (TLocal({ v_name = __keys__ }), ::(e2, []))
				: var racc = alloc_reg(ctx, KType(type_path(ctx, (new Tuple([], "Array")))));
			var rcounter = alloc_reg(ctx, KInt);
			var rtmp = alloc_reg(ctx, KDynamic);
			write(ctx, HSmallInt(0));
			set_reg(ctx, rcounter);
			write(ctx, HArray(0));
			set_reg(ctx, racc);
			gen_expr(ctx, True, e2);
			set_reg(ctx, rtmp);
			var start = jump(ctx, J3Always);
			var loop = jump_back(ctx);
			write(ctx, HReg(racc.rid));
			write(ctx, HReg(rtmp.rid));
			write(ctx, HReg(rcounter.rid));
			switch (e.eexpr) {
			case TLocal({ v_name = __foreach__ }): write(ctx, HForEach);
			case TLocal({ v_name = __hkeys__ }): write(ctx, HForIn);
				write(ctx, HSmallInt(1));
				write(ctx, HCallProperty(as3("substr"), 1));
			case _: write(ctx, HForIn);
			};
			write(ctx, HCallPropVoid(as3("push"), 1));
			start([]);
			write(ctx, HNext(rtmp.rid, rcounter.rid));
			loop(J3True);
			write(ctx, HReg(racc.rid));
			free_reg(ctx, rtmp);
			free_reg(ctx, rcounter);
			free_reg(ctx, racc);
		case (TLocal({ v_name = __new__ }), ::(e, el)): gen_expr(ctx, True, e);
			List.iter(gen_expr(ctx, True), el);
			write(ctx, HConstruct(List.length(el)));
		case (TLocal({ v_name = __delete__ }), ::(o, ::(f, []))): gen_expr(ctx, True, o);
			gen_expr(ctx, True, f);
			write(ctx, HDeleteProp(dynamic_prop));
		case (TLocal({ v_name = __unprotect__ }), ::(e, [])): write(ctx, HGetLex(type_path(ctx, (new Tuple(::("flash", []),
					"Boot")))));
			gen_expr(ctx, True, e);
			write(ctx, HCallProperty(ident("__unprotect__"), 1));
		case (TLocal({ v_name = __typeof__ }), ::(e, [])): gen_expr(ctx, True, e);
			write(ctx, HTypeof);
		case (TLocal({ v_name = __in__ }), ::(e, ::(f, []))): gen_expr(ctx, True, e);
			gen_expr(ctx, True, f);
			write(ctx, HOp(A3OIn));
		case (TLocal({ v_name = __resources__ }), []): var count = ref(0);
			Hashtbl.iter(function name: function data: incr(count);
						 write(ctx, HString("name"));
						 write(ctx, HString(name));
						 write(ctx, HObject(1)), ctx.com.resources);
			write(ctx, HArray(count.val));
		case (TLocal({ v_name = __vmem_set__ }), ::({ eexpr = TConst(TInt(code)) }, ::(e1, ::(e2, [])))): gen_expr(ctx, True, e2);
			gen_expr(ctx, True, e1);
			write(ctx, HOp(switch (code) {
		case undefined: A3OMemSet8;
		case undefined: A3OMemSet16;
		case undefined: A3OMemSet32;
		case undefined: A3OMemSetFloat;
		case undefined: A3OMemSetDouble;
		case _: assert False;
		}));
		case (TLocal({ v_name = __vmem_get__ }), ::({ eexpr = TConst(TInt(code)) }, ::(e, []))): gen_expr(ctx, True, e);
			write(ctx, HOp(switch (code) {
		case undefined: A3OMemGet8;
		case undefined: A3OMemGet16;
		case undefined: A3OMemGet32;
		case undefined: A3OMemGetFloat;
		case undefined: A3OMemGetDouble;
		case _: assert False;
		}));
		case (TLocal({ v_name = __vmem_sign__ }), ::({ eexpr = TConst(TInt(code)) }, ::(e, []))): gen_expr(ctx, True, e);
			write(ctx, HOp(switch (code) {
		case undefined: A3OSign1;
		case undefined: A3OSign8;
		case undefined: A3OSign16;
		case _: assert False;
		}));
		case (TLocal({ v_name = __vector__ }), ::(ep, [])): gen_type(ctx, type_id(ctx, r));
			write(ctx, HGetGlobalScope);
			gen_expr(ctx, True, ep);
			write(ctx, HCallStack(1));
		case (TArray({ eexpr = TLocal({ v_name = __global__ }) }, { eexpr = TConst(TString(s)) }), _):
			switch (gen_access(ctx, e, Read)) {
			case VGlobal(id): write(ctx, HFindPropStrict(id));
				List.iter(gen_expr(ctx, True), el);
				write(ctx, HCallProperty(id, List.length(el)));
			case _: assert False;
			};
		case (TConst(TSuper), _): write(ctx, HThis);
			List.iter(gen_expr(ctx, True), el);
			write(ctx, HConstructSuper(List.length(el)));
		case (TField({ eexpr = TConst(TSuper) }, f), _): var id = ident(field_name(f));
			write(ctx, HFindPropStrict(id));
			List.iter(gen_expr(ctx, True), el);
			write(ctx, HCallSuper(id, List.length(el)));
			coerce(ctx, classify(ctx, r));
		case (TField({ eexpr = TConst(TThis) }, f), _) if (!(ctx.in_static)): var id = ident(field_name(f));
			write(ctx, HFindProp(id));
			List.iter(gen_expr(ctx, True), el);
			if (retval) {
				write(ctx, HCallProperty(id, List.length(el)));
				coerce(ctx, classify(ctx, r));
			} else {
				write(ctx, HCallPropVoid(id, List.length(el)));
			};
		case (TField(e1, f), _): var old = ctx.for_call;
			ctx.for_call = True;
			gen_expr(ctx, True, e1);
			var Tuple(id, _, _) = property(ctx, field_name(f), e1.etype);
			ctx.for_call = old;
			List.iter(gen_expr(ctx, True), el);
			if (retval) {
				write(ctx, HCallProperty(id, List.length(el)));
				coerce(ctx, classify(ctx, r));
			} else {
				write(ctx, HCallPropVoid(id, List.length(el)));
			};
		case _: gen_expr(ctx, True, e);
			write(ctx, HGetGlobalScope);
			List.iter(gen_expr(ctx, True), el);
			write(ctx, HCallStack(List.length(el)));
			coerce(ctx, classify(ctx, r));
		};
	};

	public static function gen_unop(ctx, retval, op, flag, e) return {
		var k = classify(ctx, e.etype);
		switch (op) {
		case Not: gen_expr(ctx, True, e);
			write(ctx, HOp(A3ONot));
		case Neg: gen_expr(ctx, True, e);
			write(ctx, HOp(if ( = (k, KInt)) {
			A3OINeg;
		} else {
			A3ONeg;
		}));
		case NegBits: gen_expr(ctx, True, e);
			write(ctx, HOp(A3OBitNot));
		case Increment | Decrement: var incr = = (op, Increment);
			var r = switch (e.eexpr) {
			case TLocal(v): get_local_register(ctx, v);
			case _: None;
			};
			switch (r) {
			case Some(r) if (=(r.rtype, KInt)):
				if (!(r.rinit)) {
					r.rcond = True;
				} else {
					[];
				};
				if ( && (retval, = (flag, Postfix))) {
					getvar(ctx, VReg(r));
				} else {
					[];
				};
				write(ctx, if (incr) {
				HIncrIReg(r.rid);
				} else {
					HDecrIReg(r.rid);
				});
				if ( && (retval, = (flag, Prefix))) {
					getvar(ctx, VReg(r));
				} else {
					[];
				};
			case _: var Tuple(acc_read, acc_write) = gen_access_rw(ctx, e);
				var op = switch ((new Tuple(k, incr))) {
				case (KInt, True): A3OIIncr;
				case (KInt, False): A3OIDecr;
				case (_, True): A3OIncr;
				case (_, False): A3ODecr;
				};
				getvar(ctx, acc_read);
				switch (flag) {
				case Postfix if (retval): var r = alloc_reg(ctx, k);
					write(ctx, HDup);
					set_reg(ctx, r);
					write(ctx, HOp(op));
					setvar(ctx, acc_write, None);
					write(ctx, HReg(r.rid));
					free_reg(ctx, r);
				case Postfix | Prefix: write(ctx, HOp(op));
					setvar(ctx, acc_write, if (retval) {
					Some(k);
					} else {
						None;
					});
				};
			};
		};
	};

	public static function check_binop(ctx, e1, e2) return {
		var invalid = switch ((new Tuple(classify(ctx, e1.etype), classify(ctx, e2.etype)))) {
		case (KInt, KUInt) | (KUInt, KInt): switch ((new Tuple(e1.eexpr, e2.eexpr))) {
			case (TConst(TInt(i)), _) | (_, TConst(TInt(i))): < (i, );
			case _: True;
			};
		case _: False;
		};
		if (invalid) {
			error("Comparison of Int and UInt might lead to unexpected results", punion(e1.epos, e2.epos));
		} else {
			[];
		};
	};

	public static function gen_binop(ctx, retval, op, e1, e2, t, p) return {
		function write_op(op) return {
			var iop = switch (op) {
			case OpAdd: Some(A3OIAdd);
			case OpSub: Some(A3OISub);
			case OpMult: Some(A3OIMul);
			case _: None;
			};
			var op = switch (op) {
			case OpAdd: A3OAdd;
			case OpSub: A3OSub;
			case OpMult: A3OMul;
			case OpDiv: A3ODiv;
			case OpAnd: A3OAnd;
			case OpOr: A3OOr;
			case OpXor: A3OXor;
			case OpShl: A3OShl;
			case OpShr: A3OShr;
			case OpUShr: A3OUShr;
			case OpMod: A3OMod;
			case _: assert False;
			};
			switch (iop) {
			case Some(iop): var k1 = classify(ctx, e1.etype);
				var k2 = classify(ctx, e2.etype);
				switch ((new Tuple(k1, k2))) {
				case (KInt, KInt) | (KUInt, KUInt) | (KInt, KUInt) | (KUInt, KInt): write(ctx, HOp(iop));
					var ret = classify(ctx, t);
					if (<>(ret, KInt)) {
						coerce(ctx, ret);
					} else {
						[];
					};
				case _: write(ctx, HOp(op));
					if ( = (op, A3OAdd)) {
						coerce(ctx, classify(ctx, t));
					} else {
						[];
					};
				};
			case _: write(ctx, HOp(op));
				if ( && ( = (op, A3OMod), && ( = (classify(ctx, e1.etype), KInt), = (classify(ctx, e2.etype), KInt)))) {
					coerce(ctx, classify(ctx, t));
				} else {
					[];
				};
			};
		};
		function gen_op(o) return {
			check_binop(ctx, e1, e2);
			gen_expr(ctx, True, e1);
			gen_expr(ctx, True, e2);
			write(ctx, HOp(o));
		};
		function gen_eq([]) return {
			switch (is_special_compare(e1, e2)) {
			case None: gen_op(A3OEq);
			case Some(c): var f = FStatic(c, try {
					PMap.find("compare", c.cl_statics);
				} catch (e: Not_found) {
					assert False;
				});
				gen_expr(ctx, True, mk(TCall(mk(TField(mk(TTypeExpr(TClassDecl(c)), t_dynamic, p), f), t_dynamic, p), ::(e1, ::(e2, []))),
									   ctx.com.basic.tbool, p));
			};
		};
		switch (op) {
		case OpAssign: var acc = gen_access(ctx, e1, Write);
			gen_expr(ctx, True, e2);
			setvar(ctx, acc, if (retval) {
			Some(classify(ctx, e1.etype));
			} else {
				None;
			});
		case OpBoolAnd: write(ctx, HFalse);
			var j = jump_expr(ctx, e1, False);
			var b = begin_branch(ctx);
			write(ctx, HPop);
			gen_expr(ctx, True, e2);
			coerce(ctx, KBool);
			j([]);
			b([]);
		case OpBoolOr: write(ctx, HTrue);
			var j = jump_expr(ctx, e1, True);
			var b = begin_branch(ctx);
			write(ctx, HPop);
			gen_expr(ctx, True, e2);
			coerce(ctx, KBool);
			j([]);
			b([]);
		case OpAssignOp(op): var Tuple(racc, wacc) = gen_access_rw(ctx, e1);
			getvar(ctx, racc);
			gen_expr(ctx, True, e2);
			write_op(op);
			setvar(ctx, wacc, if (retval) {
			Some(classify(ctx, e1.etype));
			} else {
				None;
			});
		case OpAdd | OpMult | OpDiv | OpSub | OpAnd | OpOr | OpXor | OpMod: gen_expr(ctx, True, e1);
			gen_expr(ctx, True, e2);
			write_op(op);
		case OpShl | OpShr | OpUShr: gen_expr(ctx, True, e1);
			gen_expr(ctx, True, e2);
			write_op(op);
			coerce(ctx, classify(ctx, e1.etype));
		case OpEq: gen_eq([]);
		case OpNotEq: gen_eq([]);
			write(ctx, HOp(A3ONot));
		case OpGt: gen_op(A3OGt);
		case OpGte: gen_op(A3OGte);
		case OpLt: gen_op(A3OLt);
		case OpLte: gen_op(A3OLte);
		case OpInterval | OpArrow: assert False;
		};
	};

	public static function gen_expr(ctx, retval, e) return {
		var old = ctx.infos.istack;
		debug_infos(ctx, e.epos);
		gen_expr_content(ctx, retval, e);
		if (<>(old, ctx.infos.istack)) {
			if (<>(+(old, 1), ctx.infos.istack)) {
				stack_error(e.epos);
			} else {
				[];
			};
			if (!(retval)) {
				write(ctx, HPop);
			} else {
				[];
			};
		} else {
			if (retval) {
				stack_error(e.epos);
			} else {
				[];
			};
		};
	};

	public static function generate_function(ctx, fdata, stat) return {
		var f = begin_fun(ctx, fdata.tf_args, fdata.tf_type, ::(fdata.tf_expr, []), stat, fdata.tf_expr.epos);
		gen_expr(ctx, False, fdata.tf_expr);
		switch (follow(fdata.tf_type)) {
		case TEnum({ e_path = ([], Void) }, []) | TAbstract({ a_path = ([], Void) }, []): debug_infos(ctx, is_min = False,
			fdata.tf_expr.epos);
			write(ctx, HRetVoid);
		case _: 	function loop(e) return {
				switch (e.eexpr) {
				case TBlock([]): False;
				case TBlock(l): loop(List.hd(List.rev(l)));
				case TReturn(None): True;
				case TReturn(Some(e)): 	function inner_loop(e) return {
						switch (e.eexpr) {
						case TSwitch(_) | TFor(_) | TWhile(_) | TTry(_): False;
						case TIf(_): loop(e);
						case TParenthesis(e) | TMeta(_, e): inner_loop(e);
						case _: True;
						};
					};
					inner_loop(e);
				case TIf(_, e1, Some(e2)): && (loop(e1), loop(e2));
				case TParenthesis(e) | TMeta(_, e): loop(e);
				case _: False;
				};
			};
			if (!(loop(fdata.tf_expr))) {
				write(ctx, HRetVoid);
			} else {
				[];
			};
		};
		f([]);
	};

	public static function jump_expr_gen(ctx, e, jif, jfun) return {
		switch (e.eexpr) {
		case TParenthesis(e) | TMeta(_, e): jump_expr_gen(ctx, e, jif, jfun);
		case TBinop(op, e1, e2): 	function j(t, f) return {
				check_binop(ctx, e1, e2);
				gen_expr(ctx, True, e1);
				gen_expr(ctx, True, e2);
				jfun(if (jif) {
				t;
			} else {
				f;
			});
			};
			switch (op) {
			case OpEq if (=(is_special_compare(e1, e2), None)): j(J3Eq, J3Neq);
			case OpNotEq if (=(is_special_compare(e1, e2), None)): j(J3Neq, J3Eq);
			case OpGt: j(J3Gt, J3NotGt);
			case OpGte: j(J3Gte, J3NotGte);
			case OpLt: j(J3Lt, J3NotLt);
			case OpLte: j(J3Lte, J3NotLte);
			case _: gen_expr(ctx, True, e);
				jfun(if (jif) {
				J3True;
			} else {
				J3False;
			});
			};
		case _: gen_expr(ctx, True, e);
			jfun(if (jif) {
			J3True;
		} else {
			J3False;
		});
		};
	};

	public static function jump_expr(ctx, e, jif) return {
		jump_expr_gen(ctx, e, jif, jump(ctx));
	};

	public static function do_debug(ctx, meta) return {
		var old = ctx.debug;
		ctx.debug = && ( || (old, Meta.has(Meta.Debug, meta)), !(Meta.has(Meta.NoDebug, meta)));
		function []: ctx.debug = old;
	};

	public static function generate_method(ctx, fdata, stat, fmeta) return {
		var old = do_debug(ctx, fmeta);
		var m = generate_function(ctx, fdata, stat);
		old([]);
		m;
	};

	public static function generate_construct(ctx, fdata, c) return {
		var cargs = if (!(ctx.need_ctor_skip)) {
			fdata.tf_args;
		} else {
			List.map(function (v, c): var c = switch (c) {
		case Some(_): c;
			case None: Some(switch (classify(ctx, v.v_type)) {
			case KInt | KUInt: TInt();
				case KFloat: TFloat("0");
				case KBool: TBool(False);
				case KType(_) | KDynamic | KNone: TNull;
				});
			};
			(new Tuple(v, c)), fdata.tf_args);
		};
		var f = begin_fun(ctx, cargs, fdata.tf_type, ::(ethis, ::(fdata.tf_expr, [])), False, fdata.tf_expr.epos);
		if (ctx.need_ctor_skip) {
			switch (c.cl_kind) {
			case KGenericInstance(_): [];
			case _ if (!(Codegen.constructor_side_effects(fdata.tf_expr))): [];
			case _: var id = ident("skip_constructor");
				getvar(ctx, VGlobal(type_path(ctx, (new Tuple(::("flash", []), "Boot")))));
				getvar(ctx, VId(id));
				var j = jump(ctx, J3False);
				write(ctx, HRetVoid);
				j([]);
			};
		} else {
			[];
		};
		PMap.iter(function _: function f:
		switch ((new Tuple(f.cf_expr, f.cf_kind))) {
	case (Some({ eexpr = TFunction(fdata) }), Method(MethDynamic)): var id = ident(f.cf_name);
			write(ctx, HFindProp(id));
			write(ctx, HGetProp(id));
			var j = jump(ctx, J3True);
			write(ctx, HFindProp(id));
			write(ctx, HFunction(generate_method(ctx, fdata, False, [])));
			write(ctx, HInitProp(id));
			j([]);
		case _: [];
		}, c.cl_fields);
		gen_expr(ctx, False, fdata.tf_expr);
		debug_infos(ctx, is_min = False, fdata.tf_expr.epos);
		write(ctx, HRetVoid);
		(new Tuple(f([]), List.length(fdata.tf_args)));
	};

	public static function is_const(e) return {
		switch (e.eexpr) {
		case TConst(_): True;
		case TArrayDecl(el) | TBlock(el): List.for_all(is_const, el);
		case TObjectDecl(fl): List.for_all(function (_, e): is_const(e), fl);
		case TParenthesis(e) | TMeta(_, e): is_const(e);
		case TFunction(_): True;
		case _: False;
		};
	};

	public static function generate_class_statics(ctx, c, const) return {
		List.iter(function f:
		switch (f.cf_expr) {
	case Some({ eexpr = TFunction(_) }) if (switch (f.cf_kind) {
			case Method(MethNormal | MethInline): True;
				case _: False;
				}): [];
		case Some(e) if (=(is_const(e), const)): write(ctx, HGetLex(type_path(ctx, c.cl_path)));
			gen_expr(ctx, True, e);
			if (Codegen.is_volatile(f.cf_type)) {
				write(ctx, HArray(1));
			} else {
				[];
			};
			write(ctx, HInitProp(ident(f.cf_name)));
		case _: [];
		}, c.cl_ordered_statics);
	};

	public static function need_init(ctx, c) return {
		&& (!(ctx.swc), && (!(c.cl_extern), List.exists(function f:
		switch (f.cf_expr) {
	case Some(e): !(is_const(e));
		case _: False;
		}, c.cl_ordered_statics)));
	};

	public static function generate_extern_inits(ctx) return {
		List.iter(function t:
		switch (t) {
	case TClassDecl(c) if (c.cl_extern):
			switch (c.cl_init) {
			case None: [];
			case Some(e): gen_expr(ctx, False, e);
			};
		case _: [];
		}, ctx.com.types);
	};

	public static function generate_inits(ctx) return {
		var finit = begin_fun(ctx, [], ctx.com.basic.tvoid, [], True, null_pos);
		if (!(ctx.swc)) {
			generate_extern_inits(ctx);
		} else {
			[];
		};
		List.iter(function t:
		switch (t) {
	case TClassDecl(c) if (need_init(ctx, c)): var id = ident("init__");
			getvar(ctx, VGlobal(type_path(ctx, c.cl_path)));
			getvar(ctx, VId(id));
			var j = jump(ctx, J3True);
			getvar(ctx, VGlobal(type_path(ctx, c.cl_path)));
			write(ctx, HTrue);
			setvar(ctx, VId(id), None);
			var branch = begin_branch(ctx);
			generate_class_statics(ctx, c, False);
			branch([]);
			j([]);
		case _: [];
		}, ctx.com.types);
		switch (ctx.com.main) {
		case None: [];
		case Some(e): gen_expr(ctx, False, e);
		};
		write(ctx, HRetVoid);
		finit([]);
	};

	public static function generate_class_init(ctx, c, hc) return {
		write(ctx, HGetGlobalScope);
		if (c.cl_interface) {
			write(ctx, HNull);
		} else {
			var path = switch (c.cl_super) {
			case None: (new Tuple([], "Object"));
			case Some(sup, _): sup.cl_path;
			};
			write(ctx, HGetLex(type_path(ctx, path)));
			write(ctx, HScope);
			write(ctx, HGetLex(type_path(ctx, path)));
		};
		write(ctx, HClassDef(hc));
		List.iter(function f:
		switch ((new Tuple(f.cf_expr, f.cf_kind))) {
	case (Some({ eexpr = TFunction(fdata) }), Method(MethDynamic)): write(ctx, HDup);
			write(ctx, HFunction(generate_method(ctx, fdata, True, f.cf_meta)));
			write(ctx, HInitProp(ident(f.cf_name)));
		case _: [];
		}, c.cl_ordered_statics);
		if (!(c.cl_interface)) {
			write(ctx, HPopScope);
		} else {
			[];
		};
		write(ctx, HInitProp(type_path(ctx, c.cl_path)));
		if ( && (ctx.swc, = (c.cl_path, ctx.boot))) {
			generate_extern_inits(ctx);
		} else {
			[];
		};
		switch (c.cl_init) {
		case None: [];
		case Some(e): gen_expr(ctx, False, e);
			if (<>(ctx.block_vars, [])) {
				error("You can't have a local variable referenced from a closure inside __init__ [FP 10.1.53 crash]", e.epos);
			} else {
				[];
			};
		};
		generate_class_statics(ctx, c, True);
		if (ctx.swc) {
			generate_class_statics(ctx, c, False);
			if (<>(ctx.block_vars, [])) {
				error("You can't have a local variable referenced from a closure inside a static [FP 10.1.53 crash]", c.cl_pos);
			} else {
				[];
			};
		} else {
			[];
		};
	};

	public static function generate_enum_init(ctx, e, hc, meta) return {
		var path = (new Tuple([], "Object"));
		var name_id = type_path(ctx, e.e_path);
		write(ctx, HGetGlobalScope);
		write(ctx, HGetLex(type_path(ctx, path)));
		write(ctx, HScope);
		write(ctx, HGetLex(type_path(ctx, path)));
		write(ctx, HClassDef(hc));
		write(ctx, HPopScope);
		write(ctx, HInitProp(name_id));
		PMap.iter(function _: function f:
		switch (f.ef_type) {
	case TFun(_): [];
		case _: write(ctx, HGetLex(name_id));
			write(ctx, HFindPropStrict(name_id));
			write(ctx, HString(f.ef_name));
			write(ctx, HInt(f.ef_index));
			write(ctx, HNull);
			write(ctx, HConstructProperty(name_id, 3));
			write(ctx, HInitProp(ident(f.ef_name)));
		}, e.e_constrs);
		write(ctx, HGetLex(name_id));
		List.iter(function n: write(ctx, HString(n)), e.e_names);
		write(ctx, HArray(List.length(e.e_names)));
		write(ctx, HInitProp(ident("__constructs__")));
		switch (meta) {
		case None: [];
		case Some(e): write(ctx, HGetLex(name_id));
			gen_expr(ctx, True, e);
			write(ctx, HInitProp(ident("__meta__")));
		};
	};

	public static function extract_meta(meta) return {
		function loop(match) return switch (match) {
		case []: [];
		case ::((Meta.Meta, ::((ECall((EConst(Ident(n)), _), args), _), []), _), l): 	function mk_arg(Tuple(a, p)) return {
				switch (a) {
				case EConst(String(s)): (new Tuple(None, s));
				case EBinop(OpAssign, (EConst(Ident(n)), _), (EConst(String(s)), _)): (new Tuple(Some(n), s));
				case _: error("Invalid meta definition", p);
				};
			};
			::({ () with hlmeta_name = n;
				 hlmeta_data = Array.of_list(List.map(mk_arg, args))
			   }, loop(l));
		case ::(_, l): loop(l);
		};
		switch (loop(meta)) {
		case []: None;
		case l: Some(Array.of_list(l));
		};
	};

	public static function generate_field_kind(ctx, f, c, stat) return {
		function method_kind([]) return {
			function loop(match) return switch (match) {
			case []: (new Tuple(f.cf_name, MK3Normal));
			case ::((Meta.Getter, ::((EConst(Ident(f)), _), []), _), _): (new Tuple(f, MK3Getter));
			case ::((Meta.Setter, ::((EConst(Ident(f)), _), []), _), _): (new Tuple(f, MK3Setter));
			case ::(_, l): loop(l);
			};
			loop(f.cf_meta);
		};
		if (is_extern_field(f)) {
			None;
		} else {
			switch (f.cf_expr) {
			case Some({ eexpr = TFunction(fdata) }): 	function loop(c, name) return {
					switch (c.cl_super) {
					case None: False;
					case Some(c, _): || (PMap.exists(name, c.cl_fields), loop(c, name));
					};
				};
				switch (f.cf_kind) {
				case Method(MethDynamic) if (List.memq(f, c.cl_overrides)): None;
				case Var(_) | Method(MethDynamic): Some(HFVar({ () with hlv_type = Some(type_path(ctx, (new Tuple([], "Function"))));
															hlv_value = HVNone;
															hlv_const = False
																  }));
				case _: var Tuple(name, kind) = method_kind([]);
					var m = generate_method(ctx, fdata, stat, f.cf_meta);
					Some(HFMethod({ () with hlm_type = m;
									hlm_final = || (stat, Meta.has(Meta.Final, f.cf_meta));
									hlm_override = && (!(stat), || (loop(c, name), loop(c, f.cf_name)));
									hlm_kind = kind
								  }));
				};
			case _ if (&&(c.cl_interface, !(stat))):
				switch ((new Tuple(follow(f.cf_type), f.cf_kind))) {
				case (TFun(args, tret), Method(MethNormal | MethInline)): var dparams = ref(None);
					List.iter(function (_, o, t):
					switch (dparams.val) {
				case None: if (o) {
							dparams.val = Some(::(HVNone, []));
						} else {
							[];
						};
					case Some(l): dparams.val = Some(::(HVNone, l));
					}, args);
					var dparams = switch (dparams.val) {
					case None: None;
					case Some(l): Some(List.rev(l));
					};
					Some(HFMethod({ () with hlm_type = end_fun(ctx, List.map(function (a, opt, t): (new Tuple(alloc_var(a, t), if (opt) {
					Some(TNull);
					} else {
						None;
					})), args), dparams, tret);
					hlm_final = False;
					hlm_override = False;
					hlm_kind = snd(method_kind([]))
								  }));
				case _: None;
				};
			case _: Some(HFVar({ () with hlv_type = if (Codegen.is_volatile(f.cf_type)) {
				Some(type_path(ctx, (new Tuple([], "Array"))));
				} else {
					type_opt(ctx, f.cf_type);
				};
				hlv_value = HVNone;
							hlv_const = False
								   }));
			};
		};
	};

	public static function check_constructor(ctx, c, f) return {
		function loop(e) return {
			Type.iter(loop, e);
			switch (e.eexpr) {
			case TCall({ eexpr = TConst(TSuper) }, _): raise(Exit);
			case TBinop(OpAssign, { eexpr = TField({ eexpr = TConst(TThis) }, FInstance(cc, _, cf)) }, _) if (&&(!=(c, cc), switch (classify(ctx, cf.cf_type)) {
					case KFloat | KDynamic: True;
					case _: False;
					})): error("You cannot assign some super class vars before calling super[] in flash, this will reset them to default value",
						   e.epos);
			case _: [];
			};
		};
		try {
			loop(f.tf_expr);
		} catch (e: Exit) {
			[];
		};
	};

	public static function generate_class(ctx, c) return {
		var name = type_path(ctx, c.cl_path);
		ctx.cur_class = c;
		var Tuple(cid, cnargs) = switch (c.cl_constructor) {
		case None: if (c.cl_interface) {
				(new Tuple({ (empty_method(ctx, null_pos)) with hlmt_function = None }, 0));
			} else {
				generate_construct(ctx, { () with tf_args = [];
										  tf_type = ctx.com.basic.tvoid;
										  tf_expr = { () with eexpr = TBlock([]);
													  etype = ctx.com.basic.tvoid;
													  epos = null_pos
													}
										}, c);
			};
		case Some(f): switch (f.cf_expr) {
			case Some({ eexpr = TFunction(fdata) }): var old = do_debug(ctx, f.cf_meta);
				var m = generate_construct(ctx, fdata, c);
				check_constructor(ctx, c, fdata);
				old([]);
				m;
			case _: assert False;
			};
		};
		var has_protected = ref(None);
		function make_name(f, stat) return {
			function find_meta(c) return {
				try {
					var f = PMap.find(f.cf_name, if (stat) {
					c.cl_statics;
				} else {
					c.cl_fields;
				});
					if (List.memq(f, c.cl_overrides)) {
						raise(Not_found);
					} else {
						[];
					};
					f.cf_meta;
				} catch (e: Not_found) {
					switch (c.cl_super) {
					case None: [];
					case Some(_) if (stat): [];
					case Some(c, _): find_meta(c);
					};
				};
			};
			function protect([]) return {
				var p = switch (c.cl_path) {
				case ([], n): n;
				case (p, n): ^ (String.concat(".", p), ^ (":", n));
				};
				has_protected.val = Some(p);
				HMName(f.cf_name, HNProtected(p));
			};
			function loop_meta(match) return switch (match) {
			case []: if ( && (!(f.cf_public), ctx.swf_protected)) {
					protect([]);
				} else {
					ident(f.cf_name);
				};
			case ::(x, l): switch (x) {
				case (Meta.Getter | Meta.Setter, ::((EConst(Ident(f)), _), []), _): ident(f);
				case (Meta.Ns, ::((EConst(String(ns)), _), []), _): HMName(f.cf_name, HNNamespace(ns));
				case (Meta.Protected, [], _): protect([]);
				case _: loop_meta(l);
				};
			};
			if (c.cl_interface) {
				HMName(reserved(f.cf_name), HNNamespace(switch (c.cl_path) {
			case ([], n): n;
				case (l, n): ^ (String.concat(".", l), ^ (":", n));
				}));
			} else {
				loop_meta(find_meta(c));
			};
		};
		function generate_prop(f, acc, alloc_slot) return {
			switch (f.cf_kind) {
			case Method(_): acc;
			case Var(v): acc;
			};
		};
		var fields = PMap.fold(function f: function acc: var acc = generate_prop(f, acc, function []: 0);
		switch (generate_field_kind(ctx, f, c, False)) {
	case None: acc;
	case Some(k): ::({ () with hlf_name = make_name(f, False);
							   hlf_slot = 0;
							   hlf_kind = k;
							   hlf_metas = extract_meta(f.cf_meta)
							 }, acc);
		}, c.cl_fields, []);
		var fields = if (<>(c.cl_path, ctx.boot)) {
			fields;
		} else {
			::({
				() with hlf_name = make_name({
					() with cf_name = "init";
					cf_public = && (ctx.swc, ctx.swf_protected);
					cf_meta = [];
					cf_doc = None;
					cf_pos = c.cl_pos;
					cf_type = TFun([], t_dynamic);
					cf_params = [];
					cf_expr = None;
					cf_kind = Method(MethNormal);
					cf_overloads = []
				}, False);
				hlf_slot = 0;
				hlf_kind = HFMethod({
					() with hlm_type = generate_inits(ctx);
					hlm_final = False;
					hlm_override = True;
					hlm_kind = MK3Normal
				});
				hlf_metas = None
			}, fields);
		};
		var st_field_count = ref(0);
		var st_meth_count = ref(0);
		var statics = List.rev(List.fold_left(function acc: function f: var acc = generate_prop(f, acc, function []: incr(st_meth_count);
											  st_meth_count.val);
		switch (generate_field_kind(ctx, f, c, True)) {
	case None: acc;
	case Some(k): var count = switch (k) {
			case HFMethod(_): st_meth_count;
			case HFVar(_): st_field_count;
			case _: assert False;
			};
			incr(count);
			::({ () with hlf_name = make_name(f, True);
				 hlf_slot = count.val;
				 hlf_kind = k;
				 hlf_metas = extract_meta(f.cf_meta)
			   }, acc);
		}, [], c.cl_ordered_statics));
		var statics = if (!(need_init(ctx, c))) {
			statics;
		} else {
			::({
				() with hlf_name = ident("init__");
				hlf_slot = {
					incr(st_field_count);
					st_field_count.val;
				};
				hlf_kind = HFVar({ () with hlv_type = Some(type_id(ctx, ctx.com.basic.tbool));
								   hlv_value = HVNone;
								   hlv_const = False
								 });
				hlf_metas = None
			}, statics);
		};
		function is_dynamic(c) return {
			if ( || (<>(c.cl_dynamic, None), <>(c.cl_array_access, None))) {
				True;
			} else {
				switch (c.cl_super) {
				case None: False;
				case Some(c, _): is_dynamic(c);
				};
			};
		};
		{
			() with hlc_index = 0;
			hlc_name = name;
			hlc_super = if (c.cl_interface) {
				None;
			} else {
				Some(type_path(ctx, switch (c.cl_super) {
			case None: (new Tuple([], "Object"));
				case Some(c, _): c.cl_path;
				}));
			};
			hlc_sealed = !(is_dynamic(c));
			hlc_final = Meta.has(Meta.Final, c.cl_meta);
			hlc_interface = c.cl_interface;
			hlc_namespace = switch (has_protected.val) {
			case None: None;
			case Some(p): Some(HNProtected(p));
			};
			hlc_implements = Array.of_list(List.map(function (c, _):
			if (!(c.cl_interface)) {
			error("Can't implement class in Flash9", c.cl_pos);
			} else {
				[];
			};
			var Tuple(pack, name) = real_path(c.cl_path);
									HMMultiName(Some(name), ::(HNPublic(Some(String.concat(".", pack))), [])), c.cl_implements));
			hlc_construct = cid;
			hlc_fields = Array.of_list(fields);
			hlc_static_construct = empty_method(ctx, c.cl_pos);
			hlc_static_fields = Array.of_list(statics)
		};
	};

	public static function generate_enum(ctx, e, meta) return {
		var name_id = type_path(ctx, e.e_path);
		var api = ctx.com.basic;
		var f = begin_fun(ctx, ::((new Tuple(alloc_var("tag", api.tstring), None)), ::((new Tuple(alloc_var("index", api.tint), None)), ::((new Tuple(alloc_var("params", api.tarray(mk_mono([]))), None)), []))), api.tvoid, ::(ethis, []), False, e.e_pos);
		var tag_id = ident("tag");
		var index_id = ident("index");
		var params_id = ident("params");
		write(ctx, HFindProp(tag_id));
		write(ctx, HReg(1));
		write(ctx, HInitProp(tag_id));
		write(ctx, HFindProp(index_id));
		write(ctx, HReg(2));
		write(ctx, HInitProp(index_id));
		write(ctx, HFindProp(params_id));
		write(ctx, HReg(3));
		write(ctx, HInitProp(params_id));
		write(ctx, HRetVoid);
		var construct = f([]);
		var f = begin_fun(ctx, [], api.tstring, [], True, e.e_pos);
		write(ctx, HGetLex(type_path(ctx, (new Tuple(::("flash", []), "Boot")))));
		write(ctx, HThis);
		write(ctx, HCallProperty(ident("enum_to_string"), 1));
		write(ctx, HRet);
		var tostring = f([]);
		var st_field_count = ref(0);
		var st_meth_count = ref(0);
		var constrs = PMap.fold(function f: function acc: var st_count = switch (f.ef_type) {
	case TFun(_): st_meth_count;
		case _: st_field_count;
		};
		incr(st_count);
		::({ () with hlf_name = ident(f.ef_name);
			 hlf_slot = st_count.val;
		hlf_kind = switch (f.ef_type) {
	case TFun(args, _): var fdata = begin_fun(ctx, List.map(function (a, opt, t): (new Tuple(alloc_var(a, t), if (opt) {
			Some(TNull);
			} else {
				None;
			})), args), TEnum(e, []), [], True, f.ef_pos);
			write(ctx, HFindPropStrict(name_id));
			write(ctx, HString(f.ef_name));
			write(ctx, HInt(f.ef_index));
			var n = ref(0);
			List.iter(function _: incr(n);
					  write(ctx, HReg(n.val)), args);
			write(ctx, HArray(n.val));
			write(ctx, HConstructProperty(name_id, 3));
			write(ctx, HRet);
			var fid = fdata([]);
			HFMethod({ () with hlm_type = fid;
					   hlm_final = True;
					   hlm_override = False;
					   hlm_kind = MK3Normal
					 });
		case _: HFVar({ () with hlv_type = Some(name_id);
							hlv_value = HVNone;
							hlv_const = False
						  });
		};
		hlf_metas = None
		   }, acc), e.e_constrs, []);
		var constrs = switch (meta) {
		case None: constrs;
		case Some(_): incr(st_field_count);
			::({ () with hlf_name = ident("__meta__");
				 hlf_slot = st_field_count.val;
				 hlf_kind = HFVar({ () with hlv_type = None;
									hlv_value = HVNone;
									hlv_const = False
								  });
				 hlf_metas = None
			   }, constrs);
		};
		{
			() with hlc_index = 0;
			hlc_name = name_id;
			hlc_super = Some(type_path(ctx, (new Tuple([], "Object"))));
			hlc_sealed = True;
			hlc_final = True;
			hlc_interface = False;
			hlc_namespace = None;
			hlc_implements = [];
			hlc_construct = construct;
			hlc_fields = [{
				() with hlf_name = tag_id;
				hlf_slot = 0;
				hlf_kind = HFVar({
					() with hlv_type = Some(HMPath([], "String"));
					hlv_value = HVNone;
					hlv_const = False
				});
				hlf_metas = None
			};
			{
				() with hlf_name = index_id;
				hlf_slot = 0;
				hlf_kind = HFVar({
					() with hlv_type = Some(HMPath([], "int"));
					hlv_value = HVNone;
					hlv_const = False
				});
				hlf_metas = None
			};
			{
				() with hlf_name = params_id;
				hlf_slot = 0;
				hlf_kind = HFVar({
					() with hlv_type = Some(HMPath([], "Array"));
					hlv_value = HVNone;
					hlv_const = False
				});
				hlf_metas = None
			};
			{
				() with hlf_name = ident("__enum__");
				hlf_slot = 0;
				hlf_kind = HFVar({
					() with hlv_type = Some(HMPath([], "Boolean"));
					hlv_value = HVBool(True);
					hlv_const = True
				});
				hlf_metas = None
			};
			{
				() with hlf_name = ident("toString");
				hlf_slot = 0;
				hlf_kind = HFMethod({
					() with hlm_type = tostring;
					hlm_final = True;
					hlm_override = False;
					hlm_kind = MK3Normal
				});
				hlf_metas = None
			}];
			hlc_static_construct = empty_method(ctx, e.e_pos);
			hlc_static_fields = Array.of_list(List.rev(::({
				() with hlf_name = ident("__isenum");
				hlf_slot = +(st_field_count.val, 2);
				hlf_kind = HFVar({
					() with hlv_type = Some(HMPath([], "Boolean"));
					hlv_value = HVBool(True);
					hlv_const = True
				});
				hlf_metas = None
			}, ::({
				() with hlf_name = ident("__constructs__");
				hlf_slot = +(st_field_count.val, 1);
				hlf_kind = HFVar({
					() with hlv_type = Some(HMPath([], "Array"));
					hlv_value = HVNone;
					hlv_const = False
				});
				hlf_metas = None
			}, constrs))))
		};
	};

	public static function generate_type(ctx, t) return {
		switch (t) {
		case TClassDecl(c): if ( = (c.cl_path, (new Tuple(::("flash", ::("_Boot", [])), "RealBoot")))) {
				c.cl_path = ctx.boot;
			} else {
				[];
			};
			if ( && (c.cl_extern, || (<>(c.cl_path, (new Tuple([], "Dynamic"))), Meta.has(Meta.RealPath, c.cl_meta)))) {
				None;
			} else {
				var debug = do_debug(ctx, c.cl_meta);
				var hlc = generate_class(ctx, c);
				var init = begin_fun(ctx, [], ctx.com.basic.tvoid, ::(ethis, []), False, c.cl_pos);
				generate_class_init(ctx, c, hlc);
				write(ctx, HRetVoid);
				debug([]);
				Some(init([]), { () with hlf_name = type_path(ctx, c.cl_path);
								 hlf_slot = 0;
								 hlf_kind = HFClass(hlc);
								 hlf_metas = extract_meta(c.cl_meta)
							   });
			};
		case TEnumDecl(e): if (e.e_extern) {
				None;
			} else {
				var meta = Codegen.build_metadata(ctx.com, t);
				var hlc = generate_enum(ctx, e, meta);
				var init = begin_fun(ctx, [], ctx.com.basic.tvoid, ::(ethis, []), False, e.e_pos);
				generate_enum_init(ctx, e, hlc, meta);
				write(ctx, HRetVoid);
				Some(init([]), { () with hlf_name = type_path(ctx, e.e_path);
								 hlf_slot = 0;
								 hlf_kind = HFClass(hlc);
								 hlf_metas = extract_meta(e.e_meta)
							   });
			};
		case TAbstractDecl({ a_path = ([], Dynamic) } = a): generate_type(ctx, TClassDecl(mk_class(a.a_module, a.a_path,
					a.a_pos)));
		case TTypeDecl(_) | TAbstractDecl(_): None;
		};
	};

	public static function resource_path(name) return {
		(new Tuple(::("_res", []), ^ ("_", String.concat("_", ExtString.String.nsplit(name, ".")))));
	};

	public static function generate_resource(ctx, name) return {
		var c = mk_class(null_module, resource_path(name), null_pos);
		c.cl_super = Some(mk_class(null_module, (new Tuple(::("flash", ::("utils", [])), "ByteArray")), null_pos), []);
		var t = TClassDecl(c);
		switch (generate_type(ctx, t)) {
		case Some(m, f): (new Tuple(t, m, f));
		case None: assert False;
		};
	};

	public static function generate(com, boot_name) return {
		var ctx = {
			() with com = com;
			need_ctor_skip = Common.has_feature(com, "Type.createEmptyInstance");
			debug = com.Common.debug;
			cur_class = null_class;
			boot = (new Tuple([], boot_name));
			debugger = Common.defined(com, Define.Fdb);
			swc = Common.defined(com, Define.Swc);
			swf_protected = Common.defined(com, Define.SwfProtected);
			code = DynArray.create([]);
			locals = PMap.empty;
			infos = default_infos([]);
			trys = [];
			breaks = [];
			continues = [];
			block_vars = [];
			in_static = False;
			last_line = -1;
			last_file = "";
			try_scope_reg = None;
			for_call = False
		};
		var types = if ( && (ctx.swc, = (com.main_class, None))) {
			var hd = ref([]);
			var types = List.fold_left(function acc: function t:
			switch (t_path(t)) {
		case (::(flash, ::(_Boot, [])), RealBoot): hd.val = @(hd.val, ::(t, []));
				acc;
			case (::(flash, []), Boot): hd.val = ::(t, hd.val);
				acc;
			case _: ::(t, acc);
			}, [], com.types);
			@(hd.val, List.rev(types));
		} else {
			com.types;
		};
		var res = Hashtbl.fold(function name: function _: function acc: ::(generate_resource(ctx, name), acc), com.resources, []);
		var classes = List.fold_left(function acc: function t:
		switch (generate_type(ctx, t)) {
	case None: acc;
	case Some(m, f): ::((new Tuple(t, m, f)), acc);
		}, res, types);
		List.rev(classes);
	};

	public static function __init__() {
		Random.self_init([]);
		gen_expr_ref.val = gen_expr;
	}
}
;
