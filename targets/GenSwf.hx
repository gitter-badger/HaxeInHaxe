import Swf;
import As3;
import As3hl;
import Genswf9;
import Type;
import Common;
import Ast;
import TTFData;

enum Dependency_kind {
	DKInherit;
	DKExpr;
	DKType;
};

enum File_format {
	BJPG;
	BPNG;
	BGIF;
	SWAV;
	SMP3;
};

class Genswf {
	public static function make_tpath(match) return switch (match) {
	case HMPath(pack, name): var pdyn = ref(False);
		var Tuple(pack, name) = switch ((new Tuple(pack, name))) {
		case ([], void): (new Tuple([], "Void"));
		case ([], int): (new Tuple([], "Int"));
		case ([], uint): (new Tuple([], "UInt"));
		case ([], Number): (new Tuple([], "Float"));
		case ([], Boolean): (new Tuple([], "Bool"));
		case ([], Object): (new Tuple(::("flash", ::("utils", [])), "Object"));
		case ([], Function): (new Tuple(::("flash", ::("utils", [])), "Function"));
		case ([], Class) | ([], Array): pdyn.val = True;
			(new Tuple(pack, name));
		case ([], Error): (new Tuple(::("flash", ::("errors", [])), "Error"));
		case ([], XML): (new Tuple(::("flash", ::("xml", [])), "XML"));
		case ([], XMLList): (new Tuple(::("flash", ::("xml", [])), "XMLList"));
		case ([], QName): (new Tuple(::("flash", ::("utils", [])), "QName"));
		case ([], Namespace): (new Tuple(::("flash", ::("utils", [])), "Namespace"));
		case ([], RegExp): (new Tuple(::("flash", ::("utils", [])), "RegExp"));
		case (::(__AS3__, ::(vec, [])), Vector): (new Tuple(::("flash", []), "Vector"));
		case _: (new Tuple(pack, name));
		};
		{
			() with tpackage = pack;
			tname = name;
			tparams = if (pdyn.val) {
				::(TPType(CTPath({ () with tpackage = [];
								   tname = "Dynamic";
								   tparams = [];
								   tsub = None
								 })), []);
			} else {
				[];
			};
			tsub = None
		};
	case HMName(id, ns): {
		() with tpackage = switch (ns) {
		case HNInternal(Some(ns)): ExtString.String.nsplit(ns, ".");
		case HNPrivate(Some(ns)): try {
				var Tuple(file, line) = ExtString.String.split(ns, ".as$");
				::( ^ (file, ^ ("_", line)), []);
			} catch (e: _) {
				[];
			};
		case _: [];
		};
		tname = id;
		tparams = [];
		tsub = None
	};
	case HMNSAny(id): {
		() with tpackage = [];
		tname = id;
		tparams = [];
		tsub = None
	};
	case HMMultiName(_): assert False;
	case HMRuntimeName(_): assert False;
	case HMRuntimeNameLate: assert False;
	case HMMultiNameLate(_): assert False;
	case HMAttrib(_): assert False;
	case HMAny: assert False;
	case HMParams(t, params): var params = List.map(function t: TPType(CTPath(make_tpath(t))), params);
		{
			(make_tpath(t)) with tparams = params
		};
	};

	public static function make_param(cl, p) return {
		{
			() with tpackage = fst(cl);
			tname = snd(cl);
			tparams = ::(TPType(CTPath({
				() with tpackage = fst(p);
				tname = snd(p);
				tparams = [];
				tsub = None
			})), []);
			tsub = None
		};
	};

	public static function make_topt(match) return switch (match) {
	case None: {
		() with tpackage = [];
		tname = "Dynamic";
		tparams = [];
		tsub = None
	};
	case Some(t): make_tpath(t);
	};

	public static function make_type(t) return {
		CTPath(make_topt(t));
	};

	public static function make_dyn_type(t) return {
		switch (make_topt(t)) {
		case {
				tpackage = ::(flash, ::(utils, []));
				tname = Object | Function
			}: make_type(None);
		case o: CTPath(o);
		};
	};

	public static function is_valid_path(com, pack, name) return {
		function loop(match) return switch (match) {
		case []: False;
		case ::(load, l): switch (load((new Tuple(pack, name)), Ast.null_pos)) {
			case None: loop(l);
			case Some(file, (_, a)): True;
			};
		};
		var file = Printf.sprintf("%s/%s.hx", String.concat("/", pack), name);
		|| (loop(com.load_extern_type), try {
			ignore(Common.find_file(com, file));
			True;
		} catch (e: Not_found) {
			False;
		});
	};

	public static function build_class(com, c, file) return {
		var path = make_tpath(c.hlc_name);
		var pos = {
			() with pfile = ^ (file, ^ ("@", s_type_path((new Tuple(path.tpackage, path.tname)))));
			pmin = 0;
			pmax = 0
		};
		switch (path) {
		case {
				tpackage = ::(flash, ::(utils, []));
				tname = Object | Function
			}: var inf = { () with d_name = path.tname;
						   d_doc = None;
						   d_params = [];
						   d_meta = [];
						   d_flags = [];
						   d_data = CTPath({ () with tpackage = [];
											 tname = "Dynamic";
											 tparams = [];
											 tsub = None
										   })
						 };
			(new Tuple(path.tpackage, ::((new Tuple(ETypedef(inf), pos)), [])));
		case _: var flags = ::(HExtern, []);
			var flags = if (c.hlc_interface) {
				::(HInterface, flags);
			} else {
				flags;
			};
			var flags = switch (c.hlc_super) {
			case None | Some(HMPath([], Object)): flags;
			case Some(HMPath([], Function)): flags;
			case Some(s): ::(HExtends(make_tpath(s)), flags);
			};
			var flags = @(List.map(function i: var i = switch (i) {
		case HMMultiName(Some(id), ns): function loop(match) return switch (match) {
				case []: HMPath([], id);
				case ::(HNPublic(Some(ns)), _) if (is_valid_path(com, ExtString.String.nsplit(ns, "."), id)): HMPath(
						ExtString.String.nsplit(ns, "."), id);
				case ::(_, l): loop(l);
				};
				loop(ns);
			case HMPath(_): i;
			case _: assert False;
			};
			if (c.hlc_interface) {
			HExtends(make_tpath(i));
			} else {
				HImplements(make_tpath(i));
			}, Array.to_list(c.hlc_implements)), flags);
			var flags = if ( || (c.hlc_sealed, Common.defined(com, Define.FlashStrict))) {
				flags;
			} else {
				::(HImplements(make_tpath(HMPath([], "Dynamic"))), flags);
			};
			var getters = Hashtbl.create(0);
			var setters = Hashtbl.create(0);
			var override = Hashtbl.create(0);
			var is_xml = switch ((new Tuple(path.tpackage, path.tname))) {
			case (::(flash, ::(xml, [])), XML | XMLList): True;
			case _: False;
			};
			function make_field(stat, acc, f) return {
				var meta = ref([]);
				var flags = switch (f.hlf_name) {
				case HMPath(_): ::(APublic, []);
				case HMName(_, ns): switch (ns) {
					case HNPrivate(_) | HNNamespace(http://www.adobe.com/2006/flex/mx/internal): [];
						case HNNamespace(ns): if (!( || (c.hlc_interface, is_xml))) {
								meta.val = ::((new Tuple(Meta.Ns, ::(String(ns), []))), meta.val);
								} else {
									[];
								};
						::(APublic, []);
					case HNExplicit(_) | HNInternal(_) | HNPublic(_): ::(APublic, []);
						case HNStaticProtected(_) | HNProtected(_): meta.val = ::((new Tuple(Meta.Protected, [])), meta.val);
										::(APrivate, []);
							};
				case _: [];
				};
				if ( = (flags, [])) {
					acc;
				} else {
					var flags = if (stat) {
						::(AStatic, flags);
					} else {
						flags;
					};
					var name = make_tpath(f.hlf_name).tname;
					function mk_meta([]) return {
						List.map(function (s, cl): (new Tuple(s, List.map(function c: (new Tuple(EConst(c), pos)), cl), pos)), meta.val);
					};
					var cf = { () with cff_name = name;
							   cff_doc = None;
							   cff_pos = pos;
							   cff_meta = mk_meta([]);
							   cff_access = flags;
							   cff_kind = FVar(None, None)
							 };
					switch (f.hlf_kind) {
					case HFVar(v): if (v.hlv_const) {
							cf.cff_kind = FProp("default", "never", Some(make_type(v.hlv_type)), None);
						} else {
							cf.cff_kind = FVar(Some(make_dyn_type(v.hlv_type)), None);
						};
						::(cf, acc);
					case HFMethod(m) if (m.hlm_override): Hashtbl.add(override, (new Tuple(name, stat)), []);
						acc;
					case HFMethod(m): switch (m.hlm_kind) {
						case MK3Normal: var t = m.hlm_type;
							var p = ref(0);
							var pn = ref(0);
							var make_type = if ( || (stat, = (name, "new"))) {
								make_dyn_type;
							} else {
								make_type;
							};
							var args = List.map(function at: var aname = switch (t.hlmt_pnames) {
						case None: incr(pn);
								^ ("p", string_of_int(pn.val));
							case Some(l): switch (List.nth(l, p.val)) {
								case None: incr(pn);
									^ ("p", string_of_int(pn.val));
								case Some(i): i;
								};
							};
							var opt_val = switch (t.hlmt_dparams) {
						case None: None;
						case Some(l): try {
									Some(List.nth(l, +(-(p.val, List.length(t.hlmt_args)), List.length(l))));
								} catch (e: _) {
									None;
								};
							};
							incr(p);
							var t = make_type(at);
									var is_opt = ref(False);
							var def_val = switch (opt_val) {
						case None: None;
						case Some(v): var v = switch (v) {
								case HVNone | HVNull | HVNamespace(_) | HVString(_): is_opt.val = True;
									None;
								case HVBool(b): Some(Ident(if (b) {
									"true";
								} else {
									"false";
								}));
								case HVInt(i) | HVUInt(i): Some(Int(Int32.to_string(i)));
								case HVFloat(f): Some(Float(float_repres(f)));
								};
								switch (v) {
								case None: None;
								case Some(v): meta.val = ::((new Tuple(Meta.DefParam, ::(String(aname), ::(v, [])))), meta.val);
									Some(EConst(v), pos);
								};
							};
							(new Tuple(aname, is_opt.val, Some(t), def_val)), t.hlmt_args);
							var args = if (t.hlmt_var_args) {
								@(args, List.map(function _: incr(pn);
												 (new Tuple( ^ ("p", string_of_int(pn.val)), True, Some(make_type(None)), None)), ::(1, ::(2, ::(3, ::(4, ::(5, [])))))));
							} else {
								args;
							};
							var f = { () with f_params = [];
									  f_args = args;
									  f_type = Some(make_type(t.hlmt_ret));
									  f_expr = None
									};
							cf.cff_meta = mk_meta([]);
							cf.cff_kind = FFun(f);
							::(cf, acc);
						case MK3Getter: Hashtbl.add(getters, (new Tuple(name, stat)), m.hlm_type.hlmt_ret);
							acc;
						case MK3Setter: Hashtbl.add(setters, (new Tuple(name, stat)), switch (m.hlm_type.hlmt_args) {
						case ::(t, []): t;
							case _: assert False;
							});
							acc;
						};
					case _: acc;
					};
				};
			};
			var fields = if (c.hlc_interface) {
				[];
			} else {
				make_field(False, [], { () with hlf_name = HMPath([], "new");
										hlf_slot = 0;
										hlf_metas = None;
										hlf_kind = HFMethod({ () with hlm_type = { (c.hlc_construct) with hlmt_ret = Some(HMPath([], "void")) };
												   hlm_final = False;
												   hlm_override = False;
												   hlm_kind = MK3Normal
															})
									  });
			};
			var fields = Array.fold_left(make_field(False), fields, c.hlc_fields);
			var fields = Array.fold_left(make_field(True), fields, c.hlc_static_fields);
			function make_get_set(name, stat, tget, tset) return {
				var Tuple(get, set, t) = switch ((new Tuple(tget, tset))) {
				case (None, None): assert False;
				case (Some(t), None): (new Tuple(True, False, t));
				case (None, Some(t)): (new Tuple(False, True, t));
				case (Some(t1), Some(t2)): (new Tuple(True, True, if (<>(t1, t2)) {
					None;
				} else {
					t1;
				}));
				};
				var t = if ( = (name, "endian")) {
					Some(HMPath(::("flash", ::("utils", [])), "Endian"));
				} else {
					t;
				};
				var flags = ::(APublic, []);
				var flags = if (stat) {
					::(AStatic, flags);
				} else {
					flags;
				};
				{
					() with cff_name = name;
					cff_pos = pos;
					cff_doc = None;
					cff_access = flags;
					cff_meta = [];
					cff_kind = if ( && (get, set)) {
						FVar(Some(make_dyn_type(t)), None);
					} else {
						FProp(if (get) {
						"default";
					} else {
						"never";
					}, if (set) {
						"default";
					} else {
						"never";
					}, Some(make_dyn_type(t)), None);
					}
				};
			};
			var fields = Hashtbl.fold(function (name, stat): function t: function acc:
			if (Hashtbl.mem(override, (new Tuple(name, stat)))) {
			acc;
		} else {
			::(make_get_set(name, stat, Some(t), try {
				Some(Hashtbl.find(setters, (new Tuple(name, stat))));
				} catch (e: Not_found) {
					None;
				}), acc);
			}, getters, fields);
			var fields = Hashtbl.fold(function (name, stat): function t: function acc:
			if ( || (Hashtbl.mem(getters, (new Tuple(name, stat))), Hashtbl.mem(override, (new Tuple(name, stat))))) {
			acc;
		} else {
			::(make_get_set(name, stat, None, Some(t)), acc);
			}, setters, fields);
			try {
				var real_type = ref("");
				function loop(match) return switch (match) {
				case []: [];
				case ::(f, l): switch (f.cff_kind) {
					case FVar(Some(CTPath({ tpackage = []; tname = String | Int | UInt = tname })), None) if (List.mem(AStatic, f.cff_access)):

						if ( = (real_type.val, "")) {
							real_type.val = tname;
						} else {
							if (<>(real_type.val, tname)) {
								raise(Exit);
							} else {
								[];
							};
						};
						::( {
								() with ec_name = f.cff_name;
								ec_pos = pos;
								ec_args = [];
								ec_params = [];
								ec_meta = [];
								ec_doc = None;
								ec_type = None
							}, loop(l));
					case FFun({ f_args = [] }) if (=(f.cff_name, "new")): loop(l);
					case _: raise(Exit);
					};
				};
			List.iter(function case HExtends(_) | HImplements(_): raise(Exit);
						  case _: [], flags);
				var constr = loop(fields);
				var name = ^ ("fakeEnum:", String.concat(".", @(path.tpackage, ::(path.tname, []))));
				if (!(Common.raw_defined(com, name))) {
					raise(Exit);
				} else {
					[];
				};
				var enum_data = { () with d_name = path.tname;
								  d_doc = None;
								  d_params = [];
								  d_meta = ::((new Tuple(Meta.FakeEnum, ::((new Tuple(EConst(Ident(real_type.val)), pos)), []), pos)), []);
								  d_flags = ::(EExtern, []);
								  d_data = constr
								};
				(new Tuple(path.tpackage, ::((new Tuple(EEnum(enum_data), pos)), [])));
			} catch (e: Exit) {
				var class_data = { () with d_name = path.tname;
								   d_doc = None;
								   d_params = [];
				d_meta = if ( && (c.hlc_final, List.exists(function f: && (<>(f.cff_name, "new"), !(List.mem(AStatic, f.cff_access))), fields))) {
				::((new Tuple(Meta.Final, [], pos)), []);
				} else {
					[];
				};
				d_flags = flags;
						  d_data = fields
								 };
				(new Tuple(path.tpackage, ::((new Tuple(EClass(class_data), pos)), [])));
			};
		};
	};

	public static function extract_data(Tuple(_, tags)) return {
		var t = Common.timer("read swf");
		var h = Hashtbl.create(0);
		function loop_field(f) return {
			switch (f.hlf_kind) {
			case HFClass(c): var path = make_tpath(f.hlf_name);
				switch (path) {
				case {
						tpackage = [];
						tname = Float | Bool | Int | UInt | Dynamic
					}: [];
				case {
						tpackage = _;
						tname = MethodClosure
					}: [];
				case _: Hashtbl.add(h, (new Tuple(path.tpackage, path.tname)), c);
				};
			case _: [];
			};
		};
		List.iter(function t:
		switch (t.tdata) {
	case TActionScript3(_, as3): List.iter(function i: Array.iter(loop_field, i.hls_fields), As3hlparse.parse(as3));
		case _: [];
		}, tags);
		t([]);
		h;
	};

	public static function remove_debug_infos(as3) return {
		var hl = As3hlparse.parse(as3);
		var methods = Hashtbl.create(0);
		function loop_field(f) return {
			{
				(f) with hlf_kind = switch (f.hlf_kind) {
				case HFMethod(m): HFMethod({ (m) with hlm_type = loop_method(m.hlm_type) });
				case HFFunction(f): HFFunction(loop_method(f));
				case HFVar(v): HFVar(v);
				case HFClass(c): HFClass(loop_class(c));
				}
			};
		};
		function loop_class(c) return {
			c.hlc_construct = loop_method(c.hlc_construct);
			c.hlc_fields = Array.map(loop_field, c.hlc_fields);
			c.hlc_static_construct = loop_method(c.hlc_static_construct);
			c.hlc_static_fields = Array.map(loop_field, c.hlc_static_fields);
			c;
		};
		function loop_static(s) return {
			{
				() with hls_method = loop_method(s.hls_method);
				hls_fields = Array.map(loop_field, s.hls_fields)
			};
		};
		function loop_method(m) return {
			try {
				Hashtbl.find(methods, m.hlmt_index);
			} catch (e: Not_found) {
				var m2 = {
					(m) with hlmt_debug_name = None;
					hlmt_pnames = None
				};
				Hashtbl.add(methods, m.hlmt_index, m2);
				m2.hlmt_function = switch (m.hlmt_function) {
				case None: None;
				case Some(f): Some(loop_function(f));
				};
				m2;
			};
		};
		function loop_function(f) return {
			var cur = ref(0);
			var positions = MultiArray.map(function op: var p = cur.val;
			switch (op) {
		case HDebugReg(_) | HDebugLine(_) | HDebugFile(_) | HBreakPointLine(_) | HTimestamp: [];
			case _: incr(cur);
			};
			p, f.hlf_code);
			MultiArray.add(positions, cur.val);
			var code = MultiArray.create([]);
			MultiArray.iteri(function pos: function op:
			switch (op) {
		case HDebugReg(_) | HDebugLine(_) | HDebugFile(_) | HBreakPointLine(_) | HTimestamp: [];
			case _: 	function p(delta) return {
					-(MultiArray.get(positions, +(pos, delta)), MultiArray.length(code));
				};
				var op = switch (op) {
				case HJump(j, delta): HJump(j, p(delta));
				case HSwitch(d, deltas): HSwitch(p(d), List.map(p, deltas));
				case HFunction(m): HFunction(loop_method(m));
				case HCallStatic(m, args): HCallStatic(loop_method(m), args);
				case HClassDef(c): HClassDef(c);
				case _: op;
				};
				MultiArray.add(code, op);
			}, f.hlf_code);
			f.hlf_code = code;
			f.hlf_trys = Array.map(function t: { (t) with hltc_start = MultiArray.get(positions, t.hltc_start);
												 hltc_end = MultiArray.get(positions, t.hltc_end);
												 hltc_handle = MultiArray.get(positions, t.hltc_handle)
											   }, f.hlf_trys);
			f;
		};
		As3hlparse.flatten(List.map(loop_static, hl));
	};

	public static function parse_swf(com, file) return {
		var t = Common.timer("read swf");
		var is_swc = || ( = (file_extension(file), "swc"), = (file_extension(file), "ane"));
		var file = try {
			Common.find_file(com, file);
		} catch (e: Not_found) {
			failwith( ^ (if (is_swc) {
			"SWC";
		} else {
			"SWF";
		}, ^ (" Library not found : ", file)));
		};
		var ch = if (is_swc) {
			var zip = Zip.open_in(file);
			try {
				var entry = Zip.find_entry(zip, "library.swf");
				var ch = IO.input_string(Zip.read_entry(zip, entry));
				Zip.close_in(zip);
				ch;
			} catch (e: _) {
				Zip.close_in(zip);
				failwith( ^ ("The input swc ", ^ (file, " is corrupted")));
			};
		} else {
			IO.input_channel(open_in_bin(file));
		};
		var Tuple(h, tags) = try {
			Swf.parse(ch);
		} catch (e: T) {
			McOr(McArr(PaId(IdUid(Out_of_memory)), ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>, <...>), ExId(<...>)))),
			McArr(PaAny, ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>, <...>), ExApp(<...>,
		<...>)))))			case Out_of_memory: failwith( ^ ("Out of memory while parsing ", file));
		case _: failwith( ^ ("The input swf ", ^ (file, " is corrupted")));
		};
		IO.close_in(ch);
		List.iter(function t:
		switch (t.tdata) {
	case TActionScript3(id, as3) if (&&(!(com.debug), =(com.display, DMNone))): t.tdata = TActionScript3(id,
					remove_debug_infos(as3));
		case _: [];
		}, tags);
		t([]);
		(new Tuple(h, tags));
	};

	public static function add_swf_lib(com, file, extern) return {
		var swf_data = ref(None);
		var swf_classes = ref(None);
		function getSWF([]) return {
			switch (swf_data.val) {
			case None: var d = parse_swf(com, file);
				swf_data.val = Some(d);
				d;
			case Some(d): d;
			};
		};
		function extract([]) return {
			switch (swf_classes.val) {
			case None: var d = extract_data(getSWF([]));
				swf_classes.val = Some(d);
				d;
			case Some(d): d;
			};
		};
		function build(cl, p) return {
			switch (try {
				Some(Hashtbl.find(extract([]), cl));
				} catch (e: Not_found) {
					None;
				}) {
			case None: None;
			case Some(c): Some(file, build_class(com, c, file));
			};
		};
		com.load_extern_type = @(com.load_extern_type, ::(build, []));
		if (!(extern)) {
			com.swf_libs = ::((new Tuple(file, getSWF, extract)), com.swf_libs);
		} else {
			[];
		};
	};

	public static function tag( ? : (ext = False), d) return {
		{
			() with tid = 0;
			textended = ext;
			tdata = d
		};
	};

	public static function convert_header(com, Tuple(w, h, fps, bg)) return {
		var high = * (max(w, h), 20);
		function loop(b) return {
			if ( > (lsl(1, b), high)) {
				b;
			} else {
				loop(+(b, 1));
			};
		};
		var bits = loop(0);
		(new Tuple({ () with h_version = Common.flash_version_tag(com.flash_version);
					 h_size = { () with rect_nbits = +(bits, 1);
								left = 0;
								top = 0;
								right = * (w, 20);
								bottom = * (h, 20)
							  };
					 h_frame_count = 1;
		h_fps = to_float16(if ( > (fps, 127.0)) {
		127.0;
	} else {
		fps;
	});
	h_compressed = !(Common.defined(com, Define.NoSwfCompress))
			   }, bg));
	};

	public static function default_header(com) return {
		convert_header(com, (new Tuple(400, 300, 30., 0xFFFFFF)));
	};

	public static function build_dependencies(t) return {
		var h = ref(PMap.empty);
		function add_path(p, k) return {
			h.val = PMap.add((new Tuple(p, k)), [], h.val);
		};
		function add_type_rec(l, t) return {
			if (List.memq(t, l)) {
				[];
			} else {
				switch (t) {
				case TEnum(e, pl): add_path(e.e_path, DKType);
					List.iter(add_type_rec(::(t, l)), pl);
				case TInst(c, pl): switch (c.cl_kind) {
					case KTypeParameter(_): [];
					case _: add_path(c.cl_path, DKType);
					};
					List.iter(add_type_rec(::(t, l)), pl);
				case TAbstract(a, pl): if (Meta.has(Meta.CoreType, a.a_meta)) {
						add_path(a.a_path, DKType);
					} else {
						[];
					};
					List.iter(add_type_rec(::(t, l)), pl);
				case TFun(pl, t2): List.iter(function (_, _, t2): add_type_rec(::(t, l), t2), pl);
					add_type_rec(::(t, l), t2);
				case TAnon(a): PMap.iter(function _: function f: add_type_rec(::(t, l), f.cf_type), a.a_fields);
				case TDynamic(t2): add_type_rec(::(t, l), t2);
				case TLazy(f): add_type_rec(l, f.val([]));
				case TMono(r): switch (r.val) {
					case None: [];
					case Some(t): add_type_rec(l, t);
					};
				case TType(tt, pl): add_type_rec(::(t, l), tt.t_type);
					List.iter(add_type_rec(::(t, l)), pl);
				};
			};
		};
		function add_type(t) return {
			add_type_rec([], t);
		};
		function add_expr(e) return {
			switch (e.eexpr) {
			case TTypeExpr(t): add_path(Type.t_path(t), DKExpr);
			case TNew(c, pl, el): add_path(c.cl_path, DKExpr);
				List.iter(add_type, pl);
				List.iter(add_expr, el);
			case TFunction(f): List.iter(function (v, _): add_type(v.v_type), f.tf_args);
				add_type(f.tf_type);
				add_expr(f.tf_expr);
			case TFor(v, e1, e2): add_type(v.v_type);
				add_expr(e1);
				add_expr(e2);
			case TVar(v, eo): add_type(v.v_type);
				switch (eo) {
				case None: [];
				case Some(e): add_expr(e);
				};
			case _: Type.iter(add_expr, e);
			};
		};
		function add_field(f) return {
			add_type(f.cf_type);
			switch (f.cf_expr) {
			case None: [];
			case Some(e): add_expr(e);
			};
		};
		function add_inherit(Tuple(c, pl)) return {
			add_path(c.cl_path, DKInherit);
			List.iter(add_type, pl);
		};
		switch (t) {
		case TClassDecl(c) if (!(c.cl_extern)): List.iter(add_field, c.cl_ordered_fields);
			List.iter(add_field, c.cl_ordered_statics);
			switch (c.cl_constructor) {
			case None: [];
			case Some(f): add_field(f);
				if (<>(c.cl_path, (new Tuple(::("flash", []), "Boot")))) {
					add_path((new Tuple(::("flash", []), "Boot")), DKExpr);
				} else {
					[];
				};
			};
			switch (c.cl_init) {
			case None: [];
			case Some(e): add_expr(e);
			};
			switch (c.cl_super) {
			case None: add_path((new Tuple([], "Object")), DKInherit);
			case Some(x): add_inherit(x);
			};
			List.iter(function (_, t):
			switch (follow(t)) {
		case TInst(c, _): List.iter(add_inherit, c.cl_implements);
			case _: [];
			}, c.cl_params);
			List.iter(add_inherit, c.cl_implements);
		case TEnumDecl(e) if (!(e.e_extern)): PMap.iter(function _: function f: add_type(f.ef_type), e.e_constrs);
		case _: [];
		};
		h.val = PMap.remove((new Tuple((new Tuple([], "Int")), DKType)), h.val);
		h.val = PMap.remove((new Tuple((new Tuple([], "Int")), DKExpr)), h.val);
		h.val = PMap.remove((new Tuple((new Tuple([], "Void")), DKType)), h.val);
		PMap.foldi(function (c, k): function []: function acc: ::((new Tuple(c, k)), acc), h.val, []);
	};

	public static function build_swc_catalog(com, types) return {
		function node(x, att, l) return {
			Xml.Element(x, att, l);
		};
		function make_path(t, sep) return {
			var Tuple(path, name) = t_path(t);
			String.concat(sep, @(path, ::(name, [])));
		};
		function make_id(path) return {
			switch (Genswf9.real_path(path)) {
			case ([], n): n;
			case (l, n): ^ (String.concat(".", l), ^ (":", n));
			};
		};
		function build_script(t) return {
			var deps = build_dependencies(t);
			node("script", ::((new Tuple("name", make_path(t, "/"))), ::((new Tuple("mod", "0")), [])), @(::(node("def", ::((new Tuple("id", make_id(t_path(t)))), []), []), ::(node("dep", ::((new Tuple("id", "AS3")), ::((new Tuple("type", "n")), [])), []), [])), List.map(function (p, k): var t = switch (k) {
		case DKInherit: "i";
		case DKExpr: switch (p) {
				case (::(flash, ::(_, _)), _): "i";
				case _: "e";
				};
			case DKType: "s";
			};
			node("dep", ::((new Tuple("id", make_id(p))), ::((new Tuple("type", t)), [])), []), deps)));
		};
		var x = node("swc", ::((new Tuple("xmlns", "http://www.adobe.com/flash/swccatalog/9")), []), ::(node("versions", [], ::(node("swc", ::((new Tuple("version", "1.2")), []), []), ::(node("haxe", ::((new Tuple("version", Printf.sprintf("%d.%.2d", / (com.version, 10000), mod(com.version, 10000)))), []), []), []))), ::(node("features", [], ::(node("feature-script-deps", [], []), ::(node("feature-files", [], []), []))), ::(node("libraries", [], ::(node("library", ::((new Tuple("path", "library.swf")), []), List.map(build_script, types)), [])), ::(node("files", [], []), [])))));
		^ ("<?xml version=\"1.0\" encoding =\"utf-8\"?>\n", Xml.to_string_fmt(x));
	};

	public static function remove_classes(toremove, lib, hcl) return {
		var lib = lib([]);
		switch (toremove.val) {
		case []: lib;
		case _: var hcl = hcl([]);
			switch (List.filter(function c: Hashtbl.mem(hcl, c), toremove.val)) {
			case []: lib;
			case classes: function tags(match) return switch (match) {
				case []: [];
				case ::(t, l): switch (t.tdata) {
					case TActionScript3(h, data): var data = As3hlparse.parse(data);
						function loop(f) return {
							switch (f.hlf_kind) {
							case HFClass(_): var path = make_tpath(f.hlf_name);
								!(List.mem((new Tuple(path.tpackage, path.tname)), classes));
							case _: True;
							};
						};
						var data = List.map(function s: {
												(s) with hls_fields = Array.of_list(List.filter(loop, Array.to_list(s.hls_fields)))
											}, data);
						var data = List.filter(function s: > (Array.length(s.hls_fields), 0), data);
						if ( = (data, [])) {
							tags(l);
						} else {
							::({ (t) with tdata = TActionScript3(h, As3hlparse.flatten(data)) }, tags(l));
						};
					case _: ::(t, tags(l));
					};
				};
				toremove.val = List.filter(function p: !(List.mem(p, classes)), toremove.val);
				(new Tuple(fst(lib), tags(snd(lib))));
			};
		};
	};

	public static function detect_format(data, p) return {
		switch (try {
			(new Tuple(data0, data1, data2));
			} catch (e: _) {
				(new Tuple('\x00', '\x00', '\x00'));
			}) {
		case ('\xFF', '\xD8', _): BJPG;
		case ('\x89', 'P', 'N'): BPNG;
		case ('R', 'I', 'F'): SWAV;
		case ('I', 'D', '3'): SMP3;
		case ('\xFF', i, _) if (=(land(int_of_char(i), 0xE2), 0xE2)): SMP3;
		case ('G', 'I', 'F'): BGIF;
		case _: error("Unknown file format", p);
		};
	};

	public static function build_swf9(com, file, swc) return {
		var boot_name = if ( || (<>(swc, None), Common.defined(com, Define.HaxeBoot))) {
			"haxe";
		} else {
			^ ("boot_", String.sub(Digest.to_hex(Digest.string(Filename.basename(file))), 0, 4));
		};
		var code = Genswf9.generate(com, boot_name);
		var code = switch (swc) {
		case Some(cat): cat.val = build_swc_catalog(com, List.map(function (t, _, _): t, code));
			List.map(function (t, m, f): var path = switch (t_path(t)) {
		case ([], name): name;
			case (path, name): ^ (String.concat("/", path), ^ ("/", name));
			};
			var init = { () with hls_method = m;
						 hls_fields = [f]
					   };
					   tag(TActionScript3(Some(1, path), As3hlparse.flatten(::(init, [])))), code);
		case None: var inits = List.map(function (_, m, f): {
												() with hls_method = m;
												hls_fields = [f]
											}, code);
			::(tag(TActionScript3(if (Common.defined(com, Define.SwfUseDoAbc)) {
			Some(1, boot_name);
			} else {
				None;
			}, As3hlparse.flatten(inits))), []);
		};
		var cid = ref(0);
		var classes = ref(::({ () with f9_cid = None;
							   f9_classname = boot_name
							 }, []));
		var res = Hashtbl.fold(function name: function data: function acc: incr(cid);
							   classes.val = ::({ () with f9_cid = Some(cid.val);
												f9_classname = s_type_path(Genswf9.resource_path(name))
												}, classes.val);
							   ::(tag(TBinaryData(cid.val, data)), acc), com.resources, []);
		function load_file_data(file, p) return {
			var file = try {
				Common.find_file(com, file);
			} catch (e: Not_found) {
				file;
			};
			if ( && ( > (String.length(file), 5), = (String.sub(file, 0, 5), "data:"))) {
				String.sub(file, 5, -(String.length(file), 5));
			} else {
				try {
					Std.input_file(bin = True, file);
				} catch (e: T) {
					McOr(McArr(PaApp(PaId(IdUid(<...>)), PaStr(String.create)), ExNil, ExApp(ExApp(ExId(<...>), ExStr(<...>)),
					ExId(IdLid(<...>)))), McArr(PaAny, ExNil, ExApp(ExApp(ExId(<...>), ExStr(<...>)),
				ExId(IdLid(<...>)))))					case Invalid_argument(String.create): error("File is too big [max 16MB allowed]", p);
				case _: error("File not found", p);
				};
			};
		};
		var bmp = List.fold_left(function acc: function t:
		switch (t) {
	case TClassDecl(c): function loop(match) return switch (match) {
			case []: acc;
			case ::((Meta.Font, ::((EConst(String(file)), p), args), _), l): var file = try {
					Common.find_file(com, file);
				} catch (e: Not_found) {
					file;
				};
				var ch = try {
					open_in_bin(file);
				} catch (e: _) {
					error("File not found", p);
				};
				var ttf = try {
					TTFParser.parse(ch);
				} catch (e: e) {
					error( ^ ("Error while parsing font ", ^ (file, ^ (" : ", Printexc.to_string(e)))), p);
				};
				close_in(ch);
				function get_string(e) return {
					switch (fst(e)) {
					case EConst(String(s)): Some(s);
					case _: raise(Not_found);
					};
				};
				var ttf_config = { () with ttfc_range_str = "";
								   ttfc_font_name = None
								 };
				switch (args) {
				case ::((EConst(String(str)), _), _): ttf_config.ttfc_range_str = str;
				case _: [];
				};
				switch (args) {
				case ::(_, ::(e, [])): switch (fst(e)) {
					case EObjectDecl(fl): try {
							ttf_config.ttfc_font_name = get_string(List.assoc("fontName", fl));
						} catch (e: Not_found) {
							[];
						};
					case _: [];
					};
				case _: [];
				};
				var ttf_swf = TTFSwfWriter.to_swf(ttf, ttf_config);
				var ch = IO.output_string([]);
				var b = IO.output_bits(ch);
				TTFSwfWriter.write_font2(ch, b, ttf_swf);
				var data = IO.close_out(ch);
				incr(cid);
				classes.val = ::({ () with f9_cid = Some(cid.val);
								   f9_classname = s_type_path(c.cl_path)
								 }, classes.val);
				::(tag(TFont3({ () with cd_id = cid.val;
								cd_data = data
							  })), loop(l));
			case ::((Meta.Bitmap, ::((EConst(String(file)), p), []), _), l): var data = load_file_data(file, p);
				incr(cid);
				classes.val = ::({ () with f9_cid = Some(cid.val);
								   f9_classname = s_type_path(c.cl_path)
								 }, classes.val);
				function raw([]) return {
					tag(TBitsJPEG2({
						() with bd_id = cid.val;
						bd_data = data;
						bd_table = None;
						bd_alpha = None;
						bd_deblock = Some(0)
					}));
				};
				var t = switch (detect_format(data, p)) {
				case BPNG: try {
						var png = Png.parse(IO.input_string(data));
						var h = Png.header(png);
						switch (h.Png.png_color) {
						case Png.ClTrueColor(Png.TBits8, Png.NoAlpha): if ( > ( * ( * (h.Png.png_width, h.Png.png_height), 4),
									Sys.max_string_length)) {
								com.warning("Flash will loose some color information for this file, add alpha channel to preserve it", p);
								raise(Exit);
							} else {
								[];
							};
							var data = Extc.unzip(Png.data(png));
							var raw_data = Png.filter(png, data);
							var cmp_data = Extc.zip(raw_data);
							tag(ext = True, TBitsLossless2({ () with bll_id = cid.val;
															 bll_format = 5;
															 bll_width = h.Png.png_width;
															 bll_height = h.Png.png_height;
															 bll_data = cmp_data
														   }));
						case _: raw([]);
						};
					} catch (e: T) {
						McOr(McArr(PaId(IdUid(Exit)), ExNil, ExApp(ExId(IdLid(<...>)), ExId(IdUid(<...>)))), McArr(PaAny, ExNil,
							ExSeq(ExSem(ExApp(<...>, <...>), ExApp(<...>, <...>)))))											case Exit: raw([]);
					case _: com.error( ^ ("Failed to decode this PNG ", file), p);
						raw([]);
					};
				case _: raw([]);
				};
				::(t, loop(l));
			case ::((Meta.Bitmap, ::((EConst(String(dfile)), p1), ::((EConst(String(afile)), p2), [])), _), l): var ddata =
					load_file_data(dfile, p1);
				var adata = load_file_data(afile, p2);
				switch (detect_format(ddata, p1)) {
				case BJPG: [];
				case _: error("RGB channel must be a JPG file", p1);
				};
				switch (detect_format(adata, p2)) {
				case BPNG: [];
				case _: error("Alpha channel must be a PNG file", p2);
				};
				var png = Png.parse(IO.input_string(adata));
				var h = Png.header(png);
				var amask = switch (h.Png.png_color) {
				case Png.ClTrueColor(Png.TBits8, Png.HaveAlpha): var data = Extc.unzip(Png.data(png));
					var raw_data = Png.filter(png, data);
					var alpha = String.make( * (h.Png.png_width, h.Png.png_height), '\000');
					for (i in /*to*/0...String.length(alpha)) {
						String.unsafe_set(alpha, i, String.unsafe_get(raw_data, lsl(i, 2)));
					};
					Extc.zip(alpha);
				case _: error("PNG file must contain 8 bit alpha channel", p2);
				};
				incr(cid);
				classes.val = ::({ () with f9_cid = Some(cid.val);
								   f9_classname = s_type_path(c.cl_path)
								 }, classes.val);
				::(tag(TBitsJPEG3({ () with bd_id = cid.val;
									bd_data = ddata;
									bd_table = None;
									bd_alpha = Some(amask);
									bd_deblock = Some(0)
								  })), loop(l));
			case ::((Meta.File, ::((EConst(String(file)), p), []), _), l): var data = load_file_data(file, p);
				incr(cid);
				classes.val = ::({ () with f9_cid = Some(cid.val);
								   f9_classname = s_type_path(c.cl_path)
								 }, classes.val);
				::(tag(TBinaryData(cid.val, data)), loop(l));
			case ::((Meta.Sound, ::((EConst(String(file)), p), []), _), l): var data = load_file_data(file, p);
				function make_flags(fmt, mono, freq, bits) return {
					var fbits = switch (freq) {
					case 5512 if (<>(fmt, 2)): 0;
					case 11025: 1;
					case 22050: 2;
					case 44100: 3;
					case _: failwith( ^ ("Unsupported frequency ", string_of_int(freq)));
					};
					var bbits = switch (bits) {
					case 8: 0;
					case 16: 1;
					case _: failwith( ^ ("Unsupported bits ", string_of_int(bits)));
					};
					lor(lor(lor(lsl(fmt, 4), lsl(fbits, 2)), lsl(bbits, 1)), if (mono) {
					0;
				} else {
					1;
				});
				};
				var Tuple(flags, samples, data) = switch (detect_format(data, p)) {
				case SWAV: try {
						var i = IO.input_string(data);
						if (<>(IO.nread(i, 4), "RIFF")) {
							raise(Exit);
						} else {
							[];
						};
						ignore(IO.nread(i, 4));
						if ( || (<>(IO.nread(i, 4), "WAVE"), <>(IO.nread(i, 4), "fmt "))) {
							raise(Exit);
						} else {
							[];
						};
						var chunk_size = IO.read_i32(i);
						if (!( || ( = (chunk_size, 0x10), || ( = (chunk_size, 0x12), = (chunk_size, 0x40))))) {
							failwith( ^ ("Unsupported chunk size ", string_of_int(chunk_size)));
						} else {
							[];
						};
						if (<>(IO.read_ui16(i), 1)) {
							failwith("Not a PCM file");
						} else {
							[];
						};
						var chan = IO.read_ui16(i);
						if ( > (chan, 2)) {
							failwith("Too many channels");
						} else {
							[];
						};
						var freq = IO.read_i32(i);
						ignore(IO.read_i32(i));
						ignore(IO.read_i16(i));
						var bits = IO.read_ui16(i);
						if (<>(chunk_size, 0x10)) {
							ignore(IO.nread(i, -(chunk_size, 0x10)));
						} else {
							[];
						};
						if (<>(IO.nread(i, 4), "data")) {
							raise(Exit);
						} else {
							[];
						};
						var data_size = IO.read_i32(i);
						var data = IO.nread(i, data_size);
						(new Tuple(make_flags(0, = (chan, 1), freq, bits), / ( * (data_size, 8), * (chan, bits)), data));
					} catch (e: T) {
						McOr(McArr(PaOrp(PaOrp(PaId(<...>), PaId(<...>)), PaApp(PaId(<...>), PaAny)), ExNil, ExApp(ExApp(ExId(<...>),
								   ExStr(<...>)), ExId(IdLid(<...>)))), McArr(PaApp(PaId(IdUid(<...>)), PaId(IdLid(<...>))), ExNil, ExApp(ExApp(ExId(<...>),
									   ExApp(<...>, <...>)), ExId(IdLid(<...>)))))														case Exit | IO.No_more_input | IO.Overflow(_):
						error("Invalid WAV file", p);
					case Failure(msg): error( ^ ("Invalid WAV file [", ^ (msg, "]")), p);
					};
				case SMP3: try {
						var sampling = ref(0);
						var mono = ref(False);
						var samples = ref(0);
						var i = IO.input_string(data);
						function read_frame([]) return {
							switch (try {
								IO.read_byte(i);
								} catch (e: IO.No_more_input) {
									-1;
								}) {
							case -1: [];
							case 0x49: if (<>(IO.nread(i, 2), "D3")) {
									raise(Exit);
								} else {
									[];
								};
								ignore(IO.read_ui16(i));
								ignore(IO.read_byte(i));
								var size = land(IO.read_byte(i), 0x7F);
								var size = lor(lsl(size, 7), land(IO.read_byte(i), 0x7F));
								var size = lor(lsl(size, 7), land(IO.read_byte(i), 0x7F));
								var size = lor(lsl(size, 7), land(IO.read_byte(i), 0x7F));
								ignore(IO.nread(i, size));
								read_frame([]);
							case 0x54: if ( = (IO.nread(i, 3), "AG+")) {
									ignore(IO.nread(i, 223));
								} else {
									ignore(IO.nread(i, 124));
								};
								read_frame([]);
							case 0xFF: var infos = IO.read_byte(i);
								var ver = land(lsr(infos, 3), 3);
								sampling.val = [11025;
												0;
												22050;
												44100]ver;
								var layer = land(lsr(infos, 1), 3);
								var bits = IO.read_byte(i);
								var bitrate = if ( = (ver, 3)) {
									[0;
									 32;
									 40;
									 48;
									 56;
									 64;
									 80;
									 96;
									 112;
									 128;
									 160;
									 192;
									 224;
									 256;
									 320;
									 -1];
								} else {
									[0;
									 8;
									 16;
									 24;
									 32;
									 40;
									 48;
									 56;
									 64;
									 80;
									 96;
									 112;
									 128;
									 144;
									 160;
									 -1];
								} lsr(bits, 4);
								var srate = [[11025;
											  12000;
											  8000;
											  -1];
											 [-1;
											  -1;
											  -1;
											  -1];
											 [22050;
											  24000;
											  16000;
											  -1];
											 [44100;
											  48000;
											  32000;
											  -1]]verland(lsr(bits, 2), 3);
								var pad = land(lsr(bits, 1), 1);
								mono.val = = (lsr(IO.read_byte(i), 6), 3);
								var bpp = if ( = (ver, 3)) {
									144;
								} else {
									72;
								};
								var size = -(+( / ( * ( * (bpp, bitrate), 1000), srate), pad), 4);
								ignore(IO.nread(i, size));
								samples.val = +(samples.val, if ( = (layer, 3)) {
								384;
							} else {
								1152;
							});
								read_frame([]);
							case _: raise(Exit);
							};
						};
						read_frame([]);
						(new Tuple(make_flags(2, mono.val, sampling.val, 16), samples.val, ^ ("\x00\x00", data)));
					} catch (e: T) {
						McOr(McArr(PaOrp(PaOrp(PaId(<...>), PaId(<...>)), PaApp(PaId(<...>), PaAny)), ExNil, ExApp(ExApp(ExId(<...>),
								   ExStr(<...>)), ExId(IdLid(<...>)))), McArr(PaApp(PaId(IdUid(<...>)), PaId(IdLid(<...>))), ExNil, ExApp(ExApp(ExId(<...>),
									   ExApp(<...>, <...>)), ExId(IdLid(<...>)))))															case Exit | IO.No_more_input | IO.Overflow(_):
						error("Invalid MP3 file", p);
					case Failure(msg): error( ^ ("Invalid MP3 file [", ^ (msg, "]")), p);
					};
				case _: error("Sound extension not supported [only WAV or MP3]", p);
				};
				incr(cid);
				classes.val = ::({ () with f9_cid = Some(cid.val);
								   f9_classname = s_type_path(c.cl_path)
								 }, classes.val);
				::(tag(TSound({ () with so_id = cid.val;
								so_flags = flags;
								so_samples = samples;
								so_data = data
							  })), loop(l));
			case ::(_, l): loop(l);
			};
			loop(c.cl_meta);
		case _: acc;
		}, [], com.types);
		var clips = ::(tag(TF9Classes(List.rev(classes.val))), []);
		@(res, @(bmp, @(code, clips)));
	};

	public static function merge(com, file, priority, Tuple(h1, tags1), Tuple(h2, tags2)) return {
		var header = if (priority) {
			{
				(h2) with h_version = max(h2.h_version, Common.flash_version_tag(com.flash_version))
			};
		} else {
			h1;
		};
		var tags1 = if (priority) {
		List.filter(function case {
				tdata = TSetBgColor(_)
			}: False;
		case _: True, tags1);
		} else {
			tags1;
		};
		var use_stage = && (priority, Common.defined(com, Define.FlashUseStage));
		var classes = ref([]);
		var nframe = ref(0);
		var tags2 = List.filter(function t:
		switch (t.tdata) {
	case TPlaceObject2(_) | TPlaceObject3(_) | TRemoveObject2(_) | TRemoveObject(_): use_stage;
		case TShowFrame: incr(nframe);
			use_stage;
		case TFilesAttributes(_) | TEnableDebugger2(_) | TScenes(_): False;
		case TMetaData(_): !(Common.defined(com, Define.SwfMetadata));
		case TSetBgColor(_): priority;
		case TExport(el) if (&&(=(nframe.val, 0), >=(com.flash_version, 9.))): var el = List.filter(
						function e: var path = parse_path(e.exp_name);
						var b = List.exists(function t: = (t_path(t), path), com.types);
			if ( && (!(b), = (fst(path), []))) {
			List.iter(function t:
			if ( = (snd(t_path(t)), snd(path))) {
				error( ^ ("Linkage name '", ^ (snd(path), ^ ("' in '", ^ (file, ^ ("' should be '", ^ (s_type_path(t_path(t)), "'")))))),
					   t_infos(t).mt_pos);
				} else {
					[];
				}, com.types);
			} else {
				[];
			};
			b, el);
			classes.val = @(classes.val, List.map(function e: {
													  () with f9_cid = Some(e.exp_id);
													  f9_classname = e.exp_name
												  }, el));
			False;
		case TF9Classes(el) if (=(nframe.val, 0)): classes.val = @(classes.val, List.filter(function e: <>(e.f9_cid, None), el));
			False;
		case _: True;
		}, tags2);
		var max_id = ref(-1);
		List.iter(SwfParser.scan(function id:
		if ( > (id, max_id.val)) {
		max_id.val = id;
	} else {
		[];
		};
		id, function id: id), tags1);
		incr(max_id);
		function loop(t) return {
			SwfParser.scan(function id: +(id, max_id.val), function id: +(id, max_id.val), t);
			switch (t.tdata) {
			case TClip(c): List.iter(loop, c.c_tags);
			case _: [];
			};
		};
		List.iter(loop, tags2);
		var classes = List.map(function e:
		switch (e.f9_cid) {
	case None: e;
	case Some(id): {
			(e) with f9_cid = Some(+(id, max_id.val))
		};
		}, classes.val);
		function loop(l1, l2) return {
			switch ((new Tuple(l1, l2))) {
			case (::( {
					tdata = TSetBgColor(_)
				} = t, l1), _) | (::({ tdata = TEnableDebugger2(_) } = t, l1), _) | (::({ tdata = TFilesAttributes(_) } = t, l1), _): ::(t, loop(l1, l2));
			case (_, ::({ tdata = TSetBgColor(_) } = t, l2)): ::(t, loop(l1, l2));
			case (::( {
								  tdata = TShowFrame
							  }, l1), ::({ tdata = TShowFrame }, l2)): ::(tag(TShowFrame), loop(l1, l2));
			case (::( {
								  tdata = TF9Classes(el)
							  }, l1), _): ::(tag(TF9Classes(@(classes, el))), loop(l1, l2));
			case (::(x, l1), ::({ tdata = TShowFrame }, _)): ::(x, loop(l1, l2));
			case (_, ::(x, l2)): ::(x, loop(l1, l2));
			case (::(x, l1), []): ::(x, loop(l1, l2));
			case ([], []): [];
			};
		};
		var tags = loop(tags1, tags2);
		(new Tuple(header, tags));
	};

	public static function generate(swf_header, com) return {
		var swc = if (Common.defined(com, Define.Swc)) {
			Some(ref(""));
		} else {
			None;
		};
		var Tuple(file, codeclip) = try {
			var Tuple(f, c) = ExtString.String.split(com.file, "@");
			(new Tuple(f, Some(c)));
		} catch (e: _) {
			(new Tuple(com.file, None));
		};
		var exports = Hashtbl.create(0);
		var toremove = ref([]);
		List.iter(function (file, lib, _): var Tuple(_, tags) = lib([]);
				  List.iter(function t:
		switch (t.tdata) {
	case TExport(l): List.iter(function e: Hashtbl.add(exports, e.exp_name, []), l);
		case TF9Classes(el): List.iter(function e: if (<>(e.f9_cid, None)) {
			List.iter(function t: var extern = switch (t) {
			case TClassDecl(c): c.cl_extern;
				case TEnumDecl(e): e.e_extern;
				case TAbstractDecl(a): False;
				case TTypeDecl(t): False;
				};
				if ( && (!(extern), = (s_type_path(t_path(t)), e.f9_classname))) {
				switch (t) {
					case TClassDecl(c): if (Meta.has(Meta.Bind, c.cl_meta)) {
							toremove.val = ::(t_path(t), toremove.val);
						} else {
							error( ^ ("Class already exists in '", ^ (file, "', use @:bind to redefine it")), t_infos(t).mt_pos);
						};
					case _: error( ^ ("Invalid redefinition of class defined in '", ^ (file, "'")), t_infos(t).mt_pos);
					};
				} else {
					[];
				}, com.types);
			} else {
				[];
			}, el);
		case _: [];
		}, tags), com.swf_libs);
		var tags = build_swf9(com, file, swc);
		var Tuple(header, bg) = switch (swf_header) {
		case None: default_header(com);
		case Some(h): convert_header(com, h);
		};
		var bg = tag(TSetBgColor({ () with cr = lsr(bg, 16);
								   cg = land(lsr(bg, 8), 0xFF);
								   cb = land(bg, 0xFF)
								 }));
		var swf_debug_password = try {
			Digest.to_hex(Digest.string(Common.defined_value(com, Define.SwfDebugPassword)));
		} catch (e: Not_found) {
			"";
		};
		var debug = if (Common.defined(com, Define.Fdb)) {
			::(tag(TEnableDebugger2(0, swf_debug_password)), []);
		} else {
			[];
		};
		var meta_data = try {
			var file = Common.defined_value(com, Define.SwfMetadata);
			var file = try {
				Common.find_file(com, file);
			} catch (e: Not_found) {
				file;
			};
			var data = try {
				Std.input_file(bin = True, file);
			} catch (e: Sys_error(_)) {
				failwith( ^ ("Metadata resource file not found : ", file));
			};
			::(tag(TMetaData(data)), []);
		} catch (e: Not_found) {
			[];
		};
		var fattr = if ( < (com.flash_version, 8.)) {
			[];
		} else {
			::(tag(TFilesAttributes({
				() with fa_network = Common.defined(com, Define.NetworkSandbox);
				fa_as3 = True;
				fa_metadata = <>(meta_data, []);
				fa_gpu = && ( > (com.flash_version, 9.), Common.defined(com, Define.SwfGpu));
				fa_direct_blt = && ( > (com.flash_version, 9.), Common.defined(com, Define.SwfDirectBlit))
			})), []);
		};
		var fattr = if (Common.defined(com, Define.AdvancedTelemetry)) {
			@(fattr, ::(tag(TUnknown(0x5D, "\x00\x00")), []));
		} else {
			fattr;
		};
		var swf_script_limits = try {
			var s = Common.defined_value(com, Define.SwfScriptTimeout);
			var i = try {
				int_of_string(s);
			} catch (e: _) {
				error("Argument to swf_script_timeout must be an integer", Ast.null_pos);
			};
			::(tag(TScriptLimits(256, if ( < (i, 0)) {
			0;
		} else {
			if ( > (i, 65535)) {
					65535;
				} else {
					i;
				};
			})), []);
		} catch (e: Not_found) {
			[];
		};
		var swf = (new Tuple(header, @(fattr, @(meta_data, @(::(bg, debug), @(swf_script_limits, @(tags, ::(tag(TShowFrame), []))))))));
		var priority = ref( = (swf_header, None));
		var swf = List.fold_left(function swf: function (file, lib, cl): var swf = merge(com, file, priority.val, swf, remove_classes(toremove, lib, cl));
								 priority.val = False;
								 swf, swf, com.swf_libs);
		var swf = switch (swf) {
		case (header, tags) if (Common.defined(com, Define.SwfPreloaderFrame)): 	function loop(l) return {
				switch (l) {
				case ::( {
						tdata = TFilesAttributes(_) | TUnknown(0x5D, \x00\x00) | TMetaData(_) | TSetBgColor(_) | TEnableDebugger2(_) | TScriptLimits(_)
					} = t, l): ::(t, loop(l));
				case ::(t, l): ::(tag(TShowFrame), ::(t, l));
				case []: [];
				};
			};
			(new Tuple({ (header) with h_frame_count = +(header.h_frame_count, 1) }, loop(tags)));
		case _: swf;
		};
		var t = Common.timer("write swf");
		var level = try {
			int_of_string(Common.defined_value(com, Define.SwfCompressLevel));
		} catch (e: Not_found) {
			9;
		};
		SwfParser.init(Extc.input_zip, Extc.output_zip(level = ));
		switch (swc) {
		case Some(cat): var ch = IO.output_strings([]);
			Swf.write(ch, swf);
			var swf = IO.close_out(ch);
			var z = Zip.open_out(file);
			Zip.add_entry(cat.val, z, "catalog.xml");
			Zip.add_entry(switch (swf) {
		case ::(s, []): s;
			case _: failwith("SWF too big for SWC");
			}, z, level = 0, "library.swf");
			Zip.close_out(z);
		case None: var ch = IO.output_channel(open_out_bin(file));
			Swf.write(ch, swf);
			IO.close_out(ch);
		};
		t([]);
	};

	public static function __init__() {
		SwfParser.init(Extc.input_zip, Extc.output_zip);
		Swf.warnings.val = False;
	}
}
;
