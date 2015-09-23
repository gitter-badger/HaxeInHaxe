import Ast;
import Type;
import Common;

class Source_writer {
	function common_ctx -> function write_func -> function close_func ->
	/*object this*/
	public var indent_str = "\t";
	public var indent = "";
	public var indents = [];
	public var just_finished_block = False;
	public function close() return {
		close_func([]);
		[];
	};
	public function write(x) return {
		write_func(x);
		just_finished_block = False;
	};
	public function indent_one() return {
		thiswrite(indent_str);
	};
	public function push_indent() return {
		indents = ::(indent_str, indents);
		indent = String.concat("", indents);
	};
	public function pop_indent() return {
		switch (indents) {
		case ::(h, tail): indents = tail;
			indent = String.concat("", indents);
		case []: indent = "/*?*/";
		};
	};
	public function write_i(x) return {
		thiswrite( ^ (indent, x));
	};
	public function get_indent() return {
		indent;
	};
	public function begin_block() return {
		thiswrite("{\n");
		thispush_indent;
	};
	public function end_block() return {
		thispop_indent;
		thiswrite_i("}\n");
		just_finished_block = True;
	};
	public function end_block_line() return {
		thispop_indent;
		thiswrite_i("}");
		just_finished_block = True;
	};
	public function terminate_line() return {
		thiswrite(if (just_finished_block) {
		"";
	} else {
		";\n";
	});
	};
	public function add_include(class_path) return {
		switch (class_path) {
		case (::(@verbatim, []), file): thiswrite( ^ ("#include \"", ^ (file, "\"\n")));
		case _: var prefix = if (should_prefix_include(class_path)) {
				"";
			} else {
				get_include_prefix(common_ctx, True);
			};
			thiswrite( ^ ("#ifndef INCLUDED_", ^ (join_class_path(class_path, "_"), "\n")));
			thiswrite( ^ ("#include <", ^ (prefix, ^ (join_class_path(class_path, "/"), ".h>\n"))));
			thiswrite("#endif\n");
		};
	}
};

typedef Context = {
	ctx_common : Common.Context,
	ctx_output : String -> Unit,
	ctx_dbgout : String -> Unit,
	ctx_writer : Source_writer,
	ctx_calling : Bool,
	ctx_assigning : Bool,
	ctx_return_from_block : Bool,
	ctx_tcall_expand_args : Bool,
	ctx_return_from_internal_node : Bool,
	ctx_debug_level : Int,
	ctx_real_this_ptr : Bool,
	ctx_real_void : Bool,
	ctx_dynamic_this_ptr : Bool,
	ctx_dump_src_pos : Unit -> Unit,
	ctx_static_id_curr : Int,
	ctx_static_id_used : Int,
	ctx_static_id_depth : Int,
	ctx_switch_id : Int,
	ctx_class_name : String,
	ctx_class_super_name : String,
	ctx_local_function_args : Hashtbl<String, String>,
	ctx_local_return_block_args : Hashtbl<String, String>,
	ctx_class_member_types : Hashtbl<String, String>,
	ctx_file_info : Ref<String>,
	ctx_for_extern : Bool
};

class /*exception*/ BreakFound {

};

class /*exception*/ PathFound {

};

enum Array_of {
	ArrayInterface(value: Int);
	ArrayData(value: String);
	ArrayObject;
	ArrayAny;
	ArrayNone;
};

enum Cppia_op {
	IaFunction;
	IaVar;
	IaToInterface;
	IaToDynArray;
	IaToDataArray;
	IaToInterfaceArray;
	IaFun;
	IaCast;
	IaBlock;
	IaBreak;
	IaContinue;
	IaIsNull;
	IaNotNull;
	IaSet;
	IaCall;
	IaCallGlobal;
	IaCallStatic;
	IaCallMember;
	IaCallSuper;
	IaCallThis;
	IaCallSuperNew;
	IaCreateEnum;
	IaADef;
	IaIf;
	IaIfElse;
	IaFStatic;
	IaFName;
	IaFThisInst;
	IaFLink;
	IaFThisName;
	IaFEnum;
	IaThrow;
	IaArrayI;
	IaPlusPlus;
	IaPlusPlusPost;
	IaMinusMinus;
	IaMinusMinusPost;
	IaNeg;
	IaBitNot;
	IaLogicNot;
	IaTVars;
	IaVarDecl;
	IaVarDeclI;
	IaNew;
	IaReturn;
	IaRetVal;
	IaPosInfo;
	IaObjDef;
	IaClassOf;
	IaWhile;
	IaFor;
	IaEnumI;
	IaSwitch;
	IaTry;
	IaImplDynamic;
	IaConstInt;
	IaConstFloat;
	IaConstString;
	IaConstFalse;
	IaConstTrue;
	IaConstNull;
	IaConsThis;
	IaConstSuper;
	IaCastInt;
	IaCastBool;
	IaInterface;
	IaClass;
	IaAccessNormal;
	IaAccessNot;
	IaAccessResolve;
	IaAccessCall;
	IaEnum;
	IaInline;
	IaMain;
	IaNoMain;
	IaResources;
	IaReso;
	IaNoCast;
	IaAccessCallNative;
	IaBinOp(value: Ast.Binop);
};

class Script_writer {
	function common_ctx -> function ctx -> function filename -> function asciiOut ->
	/*object this*/
	public var debug = asciiOut;
	public var indent_str = if (asciiOut) {
		"\t";
	} else {
		"";
	};
	public var indent = "";
	public var indents = [];
	public var just_finished_block = False;
	public var classCount = 0;
	public var return_type = TMono(ref(None));
	public var buffer = Buffer.create(0);
	public var identTable = Hashtbl.create(0);
	public var fileTable = Hashtbl.create(0);
	public var identBuffer = Buffer.create(0);
	public function stringId(name) return {
		try {
			Hashtbl.find(identTable, name);
		} catch (e: Not_found) {
			var size = Hashtbl.length(identTable);
			Hashtbl.add(identTable, name, size);
			Buffer.add_string(identBuffer, ^ (string_of_int(String.length(name)), ^ (" ", ^ (name, "\n"))));
			size;
		};
	};
	public function incClasses() return {
		classCount = +(classCount, 1);
	};
	public function stringText(name) return {
		^ (string_of_int(thisstringId(name)), " ");
	};
	public var typeTable = Hashtbl.create(0);
	public var typeBuffer = Buffer.create(0);
	public function typeId(name) return {
		var name = if ( = (name, "::hx::Class")) {
			"::Class";
		} else {
			name;
		};
		try {
			Hashtbl.find(typeTable, name);
		} catch (e: Not_found) {
			var size = Hashtbl.length(typeTable);
			Hashtbl.add(typeTable, name, size);
			Buffer.add_string(typeBuffer, ^ (string_of_int(String.length(name)), ^ (" ", ^ (name, "\n"))));
			size;
		};
	};
	public function write(str) return {
		if (asciiOut) {
			Buffer.add_string(buffer, str);
		} else {
			function push(i) return {
				Buffer.add_char(buffer, Char.chr(i));
			};
			function pushI32(i) return {
				push(Int32.to_int(Int32.logand(i, Int32.of_int(255))));
			};
			List.iter(function i:
			if ( && ( >= (Int32.compare(i, Int32.zero), 0), < (Int32.compare(i, Int32.of_int(254)), 0))) {
			pushI32(i);
			} else {
				if ( && ( >= (Int32.compare(i, Int32.zero), 0), < (Int32.compare(i, Int32.of_int(65536)), 0))) {
					push(254);
					pushI32(i);
					pushI32(Int32.shift_right(i, 8));
				} else {
					push(255);
					pushI32(i);
					pushI32(Int32.shift_right(i, 8));
					pushI32(Int32.shift_right(i, 16));
					pushI32(Int32.shift_right(i, 24));
				};
			}, List.map(Int32.of_string, Str.split(Str.regexp("[\n\t ]+"), str)));
		};
		just_finished_block = False;
	};
	public function typeTextString(typeName) return {
		^ (string_of_int(thistypeId(typeName)), " ");
	};
	public function typeText(typeT) return {
		^ (string_of_int(thistypeId(script_type_string(typeT))), " ");
	};
	public function writeType(typeT) return {
		thiswrite(thistypeText(typeT));
	};
	public function boolText(value) return {
		if (value) {
			"1";
		} else {
			"0";
		};
	};
	public function writeBool(value) return {
		thiswrite(if (value) {
		"1 ";
	} else {
		"0 ";
	});
	};
	public function staticText(value) return {
		if (value) {
			"1";
		} else {
			"0";
		};
	};
	public function writeData(str) return {
		Buffer.add_string(buffer, str);
	};
	public function wint(ival) return {
		thiswrite( ^ (string_of_int(ival), " "));
	};
	public function ident(name) return {
		thiswint(thisstringId(name));
	};
	public function instText(clazz) return {
		switch (clazz.cl_path) {
		case ([], Array): ^ (string_of_int(thistypeId("Array< ::Dynamic >")), " ");
		case _: thistypeText(TInst(clazz, []));
		};
	};
	public function instName(clazz) return {
		thiswrite(thisinstText(clazz));
	};
	public function enumText(e) return {
		thistypeText(TEnum(e, []));
	};
	public function enumName(e) return {
		thiswrite(thisenumText(e));
	};
	public function close() return {
		var out_file = open_out_bin(filename);
		output_string(out_file, if (asciiOut) {
		"CPPIA\n";
	} else {
		"CPPIB\n";
	});
		var idents = Buffer.contents(identBuffer);
		output_string(out_file, ^ (string_of_int(Hashtbl.length(identTable)), "\n"));
		output_string(out_file, idents);
		var types = Buffer.contents(typeBuffer);
		output_string(out_file, ^ (string_of_int(Hashtbl.length(typeTable)), "\n"));
		output_string(out_file, types);
		output_string(out_file, ^ (string_of_int(classCount), "\n"));
		var contents = Buffer.contents(buffer);
		output_string(out_file, contents);
		close_out(out_file);
	};
	public function fileId(file) return {
		try {
			Hashtbl.find(fileTable, file);
		} catch (e: Not_found) {
			var stripped_file = strip_file(common_ctx, file);
			var result = thisstringId(stripped_file);
			Hashtbl.add(fileTable, file, result);
			result;
		};
	};
	public function constText(c) return {
		switch (c) {
		case TInt(i): ^ (thisop(IaConstInt), Printf.sprintf("%ld ", i));
		case TFloat(f): ^ (thisop(IaConstFloat), thisstringText(f));
		case TString(s): ^ (thisop(IaConstString), thisstringText(s));
		case TBool(True): thisop(IaConstTrue);
		case TBool(False): thisop(IaConstFalse);
		case TNull: thisop(IaConstNull);
		case TThis: thisop(IaConsThis);
		case TSuper: thisop(IaConstSuper);
		};
	};
	public function get_array_type(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = ([], Array) }, ::(param, [])): var typeName = type_string_suff("", param, False);
			switch (typeName) {
			case ::String: ArrayData("String");
			case int | Float | bool | String | unsigned char: ArrayData(typeName);
			case cpp::ArrayBase | Dynamic: ArrayAny;
			case _ if (is_interface_type(param)): ArrayInterface(thistypeId(script_type_string(param)));
			case _: ArrayObject;
			};
		case TAbstract(abs, pl) if (<>(abs.a_impl, None)): thisget_array_type(Abstract.get_underlying_type(abs, pl));
		case _: ArrayNone;
		};
	};
	public function pushReturn(inType) return {
		var oldReturnType = return_type;
		return_type = inType;
		function []: return_type = oldReturnType;
	};
	public function fileText(file) return {
		string_of_int(thisfileId(file));
	};
	public function indent_one() return {
		thiswrite(indent_str);
	};
	public function push_indent() return {
		indents = ::(indent_str, indents);
		indent = String.concat("", indents);
	};
	public function pop_indent() return {
		switch (indents) {
		case ::(h, tail): indents = tail;
			indent = String.concat("", indents);
		case []: indent = "/*?*/";
		};
	};
	public function write_i(x) return {
		thiswrite( ^ (indent, x));
	};
	public function get_indent() return {
		indent;
	};
	public function begin_expr() return {
		thispush_indent;
	};
	public function end_expr() return {
		if (!(just_finished_block)) {
			thiswrite("\n");
		} else {
			[];
		};
		thispop_indent;
		just_finished_block = True;
	};
	public function op(x) return {
		switch (cppia_op_info(x)) {
			(name, index): ^ (if (debug) {
			name;
		} else {
			string_of_int(index);
			}, " ");
		};
	};
	public function writeOp(o) return {
		thiswrite(thisop(o));
	};
	public function writeOpLine(o) return {
		thiswrite( ^ (thisop(o), "\n"));
	};
	public function voidFunc(isStatic, isDynamic, funcName, fieldExpression) return {
		thiswrite( ^ (thisop(IaFunction), ^ (thisstaticText(isStatic), ^ (" ", ^ (thisboolText(isDynamic), ^ (" ", ^ (thisstringText(funcName), " ")))))));
		thiswrite( ^ (thistypeTextString("Void"), "0\n"));
		thisgen_expression(fieldExpression);
	};
	public function func(isStatic, isDynamic, funcName, ret, args, isInterface, fieldExpression) return {
		thiswrite( ^ (thisop(IaFunction), ^ (thisstaticText(isStatic), ^ (" ", ^ (thisboolText(isDynamic), ^ (" ", ^ (thisstringText(funcName), " ")))))));
		thiswrite( ^ (thistypeText(ret), ^ (string_of_int(List.length(args)), " ")));
		List.iter(function (name, opt, typ): thiswrite( ^ (thisstringText(name), ^ (thisboolText(opt), ^ (" ", ^ (thistypeText(typ), " "))))), args);
		thiswrite("\n");
		if (!(isInterface)) {
			switch (fieldExpression) {
			case Some({ eexpr = TFunction(function_def) } = e): thisgen_expression(e);
			case _: print_endline( ^ ("Missing function body for ", funcName));
			};
		} else {
			[];
		};
	};
	public function var(readAcc, writeAcc, isExtern, isStatic, name, varType, varExpr) return {
		thiswrite( ^ (thisop(IaVar), ^ (thisstaticText(isStatic), ^ (" ", ^ (thisop(readAcc), ^ (thisop(writeAcc), ^ (thisboolText(isExtern), ^ (" ", ^ (thisstringText(name), ^ (thistypeText(varType), switch (varExpr) {
	case Some(_): "1\n";
		case _: "0\n";
		}))))))))));
		switch (varExpr) {
		case Some(expression): thisgen_expression(expression);
		case _: [];
		};
	};
	public function implDynamic() return {
		thiswriteOpLine(IaImplDynamic);
	};
	public function writeVar(v) return {
		thisident(v.v_name);
		thiswint(v.v_id);
		thiswriteBool(v.v_capture);
		thiswriteType(v.v_type);
	};
	public function writeList(prefix, len) return {
		thiswrite( ^ (prefix, ^ (" ", ^ (string_of_int(len), "\n"))));
	};
	public function writePos(expr) return {
		if (debug) {
			thiswrite( ^ (thisfileText(expr.epos.pfile), ^ ("\t", ^ (string_of_int(Lexer.get_error_line(expr.epos)), indent))));
		} else {
			[];
		};
	};
	public function checkCast(toType, expr, forceCast, fromGenExpression) return {
		function write_cast(text) return {
			if (!(fromGenExpression)) {
				thiswritePos(expr);
			} else {
				[];
			};
			thiswrite( ^ (text, "\n"));
			thisbegin_expr;
			thisgen_expression(expr);
			thisend_expr;
			True;
		};
		var was_cast = if (is_interface_type(toType)) {
			if (is_dynamic_in_cppia(ctx, expr)) {
				write_cast( ^ (thisop(IaToInterface), ^ (thistypeText(toType), ^ (" ", thistypeTextString("Dynamic")))));
			} else {
				if (!(is_matching_interface_type(toType, expr.etype))) {
					write_cast( ^ (thisop(IaToInterface), ^ (thistypeText(toType), ^ (" ", thistypeText(expr.etype)))));
				} else {
					False;
				};
			};
		} else {
			function get_array_expr_type(expr) return {
				if (is_dynamic_in_cppia(ctx, expr)) {
					ArrayNone;
				} else {
					thisget_array_type(expr.etype);
				};
			};
			switch ((new Tuple(thisget_array_type(toType), get_array_expr_type(expr)))) {
			case (ArrayAny, _): False;
			case (ArrayObject, ArrayData(_)): write_cast(thisop(IaToDynArray));
			case (ArrayData(t), ArrayNone) | (ArrayData(t), ArrayObject) | (ArrayData(t), ArrayAny): write_cast( ^ (thisop(
							IaToDataArray), thistypeTextString( ^ ("Array.", t))));
			case (ArrayInterface(t), ArrayNone) | (ArrayInterface(t), ArrayAny): write_cast( ^ (thisop(IaToInterfaceArray),
						string_of_int(t)));
			case (_, _): False;
			};
		};
		if (!(was_cast)) {
			if (forceCast) {
				var op = switch (type_string(expr.etype)) {
				case int: IaCastInt;
				case bool: IaCastBool;
				case _ if (is_interface_type(toType)): IaNoCast;
				case _: IaCast;
				};
				thiswriteOpLine(op);
			} else {
				[];
			};
			thisgen_expression(expr);
		} else {
			[];
		};
	};
	public function gen_expression(expr) return {
		var expression = remove_parens(expr);
		thisbegin_expr;
		thiswritePos(expression);
		switch (expression.eexpr) {
		case TFunction(function_def): thiswrite( ^ (thisop(IaFun), ^ (thistypeText(function_def.tf_type),
			^ (string_of_int(List.length(function_def.tf_args)), "\n"))));
			List.iter(function (arg, init): thiswrite( ^ (indent, indent_str));
			thiswriteVar(arg);
			switch (init) {
		case Some(const): thiswrite( ^ ("1 ", ^ (thisconstText(const), "\n")));
			case _: thiswrite("0\n");
			}, function_def.tf_args);
			var pop = thispushReturn(function_def.tf_type);
			thisgen_expression(function_def.tf_expr);
			pop([]);
		case TBlock(expr_list): thiswriteList(thisop(IaBlock), List.length(expr_list));
			List.iter(thisgen_expression, expr_list);
		case TConst(const): thiswrite(thisconstText(const));
		case TBreak: thiswriteOp(IaBreak);
		case TContinue: thiswriteOp(IaContinue);
		case TBinop(op, e1, e2) if (=(op, OpAssign)): thiswriteOpLine(IaSet);
			thisgen_expression(e1);
			thischeckCast(e1.etype, e2, False, False);
		case TBinop(OpEq, e1, { eexpr = TConst(TNull) }): thiswriteOpLine(IaIsNull);
			thisgen_expression(e1);
		case TBinop(OpNotEq, e1, { eexpr = TConst(TNull) }): thiswriteOpLine(IaNotNull);
			thisgen_expression(e1);
		case TBinop(OpEq, { eexpr = TConst(TNull) }, e1): thiswriteOpLine(IaIsNull);
			thisgen_expression(e1);
		case TBinop(OpNotEq, { eexpr = TConst(TNull) }, e1): thiswriteOpLine(IaNotNull);
			thisgen_expression(e1);
		case TBinop(op, e1, e2): thiswriteOpLine(IaBinOp(op));
			thisgen_expression(e1);
			thisgen_expression(e2);
		case TThrow(e): thiswriteOpLine(IaThrow);
			thisgen_expression(e);
		case TArrayDecl(expr_list): thiswrite( ^ (thisop(IaADef), ^ (thistypeText(expression.etype), ^ (" ",
												   ^ (string_of_int(List.length(expr_list)), "\n")))));
			List.iter(thisgen_expression, expr_list);
		case TIf(e, e1, e2): switch (e2) {
			case None: thiswriteOpLine(IaIf);
				thisgen_expression(e);
				thisgen_expression(e1);
			case Some(elze): thiswriteOpLine(IaIfElse);
				thisgen_expression(e);
				thisgen_expression(e1);
				thisgen_expression(elze);
			};
		case TCall(func, arg_list): var argN = ^ (string_of_int(List.length(arg_list)), " ");
			function is_real_function(field) return {
				switch (field.cf_kind) {
				case Method(MethNormal) | Method(MethInline): True;
				case _: False;
				};
			};
			function gen_call([]) return {
				switch (remove_parens(func).eexpr) {
				case TField({ eexpr = TLocal({ v_name = __global__ }) }, field): thiswrite( ^ (thisop(IaCallGlobal),
					^ (thisstringText(field_name(field)), ^ (argN, "\n"))));
				case TField(obj, FStatic(class_def, field)) if (is_real_function(field)): thiswrite( ^ (thisop(IaCallStatic),
							^ (thisinstText(class_def), ^ (" ", ^ (thisstringText(field.cf_name), ^ (argN, "\n"))))));
				case TField(obj, FInstance(_, _, field)) if (&&(is_this(obj), is_real_function(field))): thiswrite( ^ (thisop(IaCallThis),
							^ (thistypeText(obj.etype), ^ (" ", ^ (thisstringText(field.cf_name), ^ (argN, "\n"))))));
				case TField(obj, FInstance(_, _, field)) if (is_super(obj)): thiswrite( ^ (thisop(IaCallSuper), ^ (thistypeText(obj.etype),
							^ (" ", ^ (thisstringText(field.cf_name), ^ (argN, "\n"))))));
				case TField(obj, FInstance(_, _, field)) if (is_real_function(field)): thiswrite( ^ (thisop(IaCallMember),
							^ (thistypeText(obj.etype), ^ (" ", ^ (thisstringText(field.cf_name), ^ (argN, "\n"))))));
					thisgen_expression(obj);
				case TField(obj, FDynamic(name)) if (||(is_internal_member(name), &&(=(type_string(obj.etype), "::String"), =(name, "cca"))))
							: thiswrite( ^ (thisop(IaCallMember), ^ (thistypeText(obj.etype), ^ (" ", ^ (thisstringText(name), ^ (argN, "\n"))))));
					thisgen_expression(obj);
				case TConst(TSuper): thiswrite( ^ (thisop(IaCallSuperNew), ^ (thistypeText(func.etype), ^ (" ", ^ (argN, "\n")))));
				case TField(_, FEnum(enum, field)): thiswrite( ^ (thisop(IaCreateEnum), ^ (thisenumText(enum), ^ (" ",
							^ (thisstringText(field.ef_name), ^ (argN, "\n"))))));
				case _: thiswrite( ^ (thisop(IaCall), ^ (argN, "\n")));
					thisgen_expression(func);
				};
				var matched_args = switch (func.etype) {
				case TFun(args, _): try {
						List.iter2(function (_, _, protoT): function arg: thischeckCast(protoT, arg, False, False), args, arg_list);
						True;
					} catch (e: Invalid_argument(_)) {
						False;
					};
				case _: False;
				};
				if (!(matched_args)) {
					List.iter(thisgen_expression, arg_list);
				} else {
					[];
				};
			};
			switch (remove_parens(func).eexpr) {
			case TField(obj, field) if (&&(is_array_or_dyn_array(obj.etype), =(field_name(field), "map"))):
				switch (thisget_array_type(expression.etype)) {
				case ArrayData(t): thiswrite( ^ (thisop(IaToDataArray), ^ (thistypeTextString( ^ ("Array.", t)), "\n")));
					thisbegin_expr;
					thiswritePos(func);
					gen_call([]);
					thisend_expr;
				case ArrayInterface(t): thiswrite( ^ (thisop(IaToInterfaceArray), ^ (string_of_int(t), "\n")));
					thisbegin_expr;
					thiswritePos(func);
					gen_call([]);
					thisend_expr;
				case _: gen_call([]);
				};
			case _: gen_call([]);
			};
		case TField(obj, acc): var typeText = thistypeText(obj.etype);
			switch (acc) {
			case FDynamic(name): thiswrite( ^ (thisop(IaFName), ^ (typeText, ^ (" ", ^ (thisstringText(name), "\n")))));
				thisgen_expression(obj);
			case FStatic(class_def, field): thiswrite( ^ (thisop(IaFStatic), ^ (thisinstText(class_def), ^ (" ",
						thisstringText(field.cf_name)))));
			case FInstance(_, _, field) if (is_this(obj)): thiswrite( ^ (thisop(IaFThisInst), ^ (typeText, ^ (" ",
						thisstringText(field.cf_name)))));
			case FInstance(_, _, field): thiswrite( ^ (thisop(IaFLink), ^ (typeText, ^ (" ", ^ (thisstringText(field.cf_name),
														"\n")))));
				thisgen_expression(obj);
			case FClosure(_, field) if (is_this(obj)): thiswrite( ^ (thisop(IaFThisName), ^ (typeText, ^ (" ",
						^ (thisstringText(field.cf_name), "\n")))));
			case FAnon(field) if (is_this(obj)): thiswrite( ^ (thisop(IaFThisName), ^ (typeText, ^ (" ",
						^ (thisstringText(field.cf_name), "\n")))));
			case FClosure(_, field) | FAnon(field): thiswrite( ^ (thisop(IaFName), ^ (typeText, ^ (" ",
						^ (thisstringText(field.cf_name), "\n")))));
				thisgen_expression(obj);
			case FEnum(enum, field): thiswrite( ^ (thisop(IaFEnum), ^ (thisenumText(enum), ^ (" ", thisstringText(field.ef_name)))));
			};
		case TArray(e1, e2): thiswrite( ^ (thisop(IaArrayI), ^ (thistypeText(e1.etype), "\n")));
			thisgen_expression(e1);
			thisgen_expression(e2);
		case TUnop(op, flag, e): thiswriteOpLine(switch ((new Tuple(op, flag))) {
		case (Increment, Prefix): IaPlusPlus;
			case (Increment, _): IaPlusPlusPost;
			case (Decrement, Prefix): IaMinusMinus;
			case (Decrement, _): IaMinusMinusPost;
			case (Not, _): IaLogicNot;
			case (Neg, _): IaNeg;
			case (NegBits, _): IaBitNot;
			});
			thisgen_expression(e);
		case TLocal(var): thiswrite( ^ (thisop(IaVar), string_of_int(var.v_id)));
		case TVar(tvar, optional_init): thiswrite( ^ (thisop(IaTVars), ^ (string_of_int(1), "\n")));
			thiswrite( ^ ("\t\t", indent));
			switch (optional_init) {
			case None: thiswriteOp(IaVarDecl);
				thiswriteVar(tvar);
			case Some(init): thiswriteOp(IaVarDeclI);
				var init = remove_parens(init);
				thiswriteVar(tvar);
				thiswrite( ^ (" ", thistypeText(init.etype)));
				thiswrite("\n");
				thischeckCast(tvar.v_type, init, False, False);
			};
		case TNew(clazz, params, arg_list): thiswrite( ^ (thisop(IaNew), ^ (thistypeText(TInst(clazz, params)),
					^ (string_of_int(List.length(arg_list)), "\n"))));
			function matched_args(clazz) return {
				switch ((new Tuple(clazz.cl_constructor, clazz.cl_super))) {
				case (None, Some(super)): matched_args(fst(super));
				case (None, _): False;
				case (Some(ctr), _): switch (ctr.cf_type) {
					case TFun(args, _): try {
							List.iter2(function (_, _, protoT): function arg: thischeckCast(protoT, arg, False, False), args, arg_list);
							True;
						} catch (e: Invalid_argument(_)) {
							False;
						};
					case _: False;
					};
				};
			};
			if (!(matched_args(clazz))) {
				List.iter(thisgen_expression, arg_list);
			} else {
				[];
			};
		case TReturn(optval): switch (optval) {
			case None: thiswriteOpLine(IaReturn);
			case Some(value): thiswrite( ^ (thisop(IaRetVal), ^ (thistypeText(value.etype), "\n")));
				thischeckCast(return_type, value, False, False);
			};
		case TObjectDecl(::((fileName, { eexpr = TConst(TString(file)) }), ::((lineNumber, { eexpr = TConst(TInt(line)) }), ::((className, { eexpr = TConst(TString(class_name)) }), ::((methodName, { eexpr = TConst(TString(meth)) }), [])))))
				: thiswrite( ^ (thisop(IaPosInfo), ^ (thisstringText(file), ^ (Printf.sprintf("%ld", line), ^ (" ",
												  ^ (thisstringText(class_name), ^ (" ", thisstringText(meth))))))));
		case TObjectDecl(values): thiswrite( ^ (thisop(IaObjDef), string_of_int(List.length(values))));
			thiswrite(" ");
			List.iter(function (name, _): thiswrite(thisstringText(name)), values);
			thiswrite("\n");
			List.iter(function (_, e): thisgen_expression(e), values);
		case TTypeExpr(type_expr): var klass = ^ ("::", join_class_path(t_path(type_expr), "::"));
			thiswrite( ^ (thisop(IaClassOf), string_of_int(thistypeId(klass))));
		case TWhile(e1, e2, flag): thiswrite( ^ (thisop(IaWhile), ^ (if ( = (flag, NormalWhile)) {
			"1";
		} else {
			"0";
		}, "\n")));
			thisgen_expression(e1);
			thisgen_expression(e2);
		case TFor(tvar, init, loop): thiswriteOp(IaFor);
			thiswriteVar(tvar);
			thiswrite("\n");
			thisgen_expression(init);
			thisgen_expression(loop);
		case TEnumParameter(expr, ef, i): var enum = switch (follow(ef.ef_type)) {
			case TEnum(en, _) | TFun(_, TEnum(en, _)): en;
				case _: assert False;
				};
				thiswrite( ^ (thisop(IaEnumI), ^ (thistypeText(TEnum(enum, [])), ^ (string_of_int(i), "\n"))));
				thisgen_expression(expr);
			case TSwitch(condition, cases, optional_default): thiswrite( ^ (thisop(IaSwitch), ^ (string_of_int(List.length(cases)),
				^ (" ", ^ (switch (optional_default) {
		case None: "0";
		case Some(_): "1";
			}, "\n")))));
			thisgen_expression(condition);
			List.iter(function (cases_list, expression): thiswriteList( ^ ("\t\t\t", indent), List.length(cases_list));
					  List.iter(function value: thisgen_expression(value), cases_list);
					  thisgen_expression(expression), cases);
			switch (optional_default) {
			case None: [];
			case Some(expr): thisgen_expression(expr);
			};
		case TTry(e, catches): thiswriteList(thisop(IaTry), List.length(catches));
			thisgen_expression(e);
			List.iter(function (tvar, catch_expr): thiswrite( ^ ("\t\t\t", indent));
					  thiswriteVar(tvar);
					  thiswrite("\n");
					  thisgen_expression(catch_expr), catches);
		case TCast(cast, None): thischeckCast(expression.etype, cast, True, True);
		case TCast(cast, Some(_)): thischeckCast(expression.etype, cast, True, True);
		case TParenthesis(_): error("Unexpected parens", expression.epos);
		case TMeta(_, _): error("Unexpected meta", expression.epos);
		};
		thisend_expr;
	}
};
class Gencpp {
	public static function unsupported(p) return {
		error("This expression cannot be generated to Cpp", p);
	};

	public static var follow = Abstract.follow_with_abstracts;

	public static function join_class_path(path, separator) return {
		var result = switch ((new Tuple(fst(path), snd(path)))) {
		case ([], s): s;
		case (el, s): ^ (String.concat(separator, el), ^ (separator, s));
		};
		if (String.contains(result, '+')) {
			var idx = String.index(result, '+');
			^ (String.sub(result, 0, idx), String.sub(result, +(idx, 1), -(-(String.length(result), idx), 1)));
		} else {
			result;
		};
	};

	public static function is_internal_class(match) return switch (match) {
	case ([], Int) | ([], Void) | ([], String) | ([], Null) | ([], Float) | ([], Array) | ([], Class) | ([], Enum) | ([], Bool) | ([], Dynamic) | ([], ArrayAccess) | (::(cpp, []), FastIterator) | (::(cpp, []), Pointer) | (::(cpp, []), ConstPointer) | (::(cpp, []), RawPointer) | (::(cpp, []), RawConstPointer) | (::(cpp, []), Function)
			: True;
	case ([], Math) | (::(haxe, ::(io, [])), Unsigned_char__): True;
	case (::(cpp, []), Int8) | (::(cpp, []), UInt8) | (::(cpp, []), Char) | (::(cpp, []), Int16) | (::(cpp, []), UInt16) | (::(cpp, []), Int32) | (::(cpp, []), UInt32) | (::(cpp, []), Int64) | (::(cpp, []), UInt64) | (::(cpp, []), Float32) | (::(cpp, []), Float64)
			: True;
	case _: False;
	};

	public static function get_include_prefix(common_ctx, with_slash) return {
		try {
			^ (Common.defined_value(common_ctx, Define.IncludePrefix), if (with_slash) {
			"/";
		} else {
			"";
		});
		} catch (e: Not_found) {
			"";
		};
	};

	public static function should_prefix_include(match) return switch (match) {
	case x if (is_internal_class(x)): False;
	case ([], hxMath): True;
	case _: False;
	};

	public static function file_source_writer(common_ctx, filename) return {
		var out_file = open_out(filename);
		new source_writer(common_ctx, output_string(out_file), function []: close_out(out_file));
	};

	public static function read_whole_file(chan) return {
		Std.input_all(chan);
	};

	public static function cached_source_writer(common_ctx, filename) return {
		try {
			var in_file = open_in(filename);
			var old_contents = read_whole_file(in_file);
			close_in(in_file);
			var buffer = Buffer.create(0);
			function add_buf(str) return {
				Buffer.add_string(buffer, str);
			};
			function close([]) return {
				var contents = Buffer.contents(buffer);
				if (!( = (contents, old_contents))) {
					var out_file = open_out(filename);
					output_string(out_file, contents);
					close_out(out_file);
				} else {
					[];
				};
			};
			new source_writer(common_ctx, add_buf, close);
		} catch (e: _) {
			file_source_writer(common_ctx, filename);
		};
	};

	public static var make_class_directories = Common.mkdir_recursive;

	public static function make_base_directory(dir) return {
		make_class_directories("", Str.split_delim(Str.regexp("[\\/]+"), dir));
	};

	public static function new_source_file(common_ctx, base_dir, sub_dir, extension, class_path) return {
		var include_prefix = get_include_prefix(common_ctx, True);
		var full_dir = if ( && ( = (sub_dir, "include"), <>(include_prefix, ""))) {
			var dir = switch (fst(class_path)) {
			case []: ^ (base_dir, ^ ("/include/", get_include_prefix(common_ctx, False)));
			case path: ^ (base_dir, ^ ("/include/", ^ (include_prefix, String.concat("/", path))));
			};
			make_base_directory(dir);
			dir;
		} else {
			make_class_directories(base_dir, ::(sub_dir, fst(class_path)));
			^ (base_dir, ^ ("/", ^ (sub_dir, ^ ("/", String.concat("/", fst(class_path))))));
		};
		cached_source_writer(common_ctx, ^ (full_dir, ^ ("/", ^ (snd(class_path), extension))));
	};

	public static function source_file_extension(common_ctx) return {
		if (Common.defined(common_ctx, Define.Objc)) {
			".mm";
		} else {
			try {
				^ (".", Common.defined_value(common_ctx, Define.FileExtension));
			} catch (e: Not_found) {
				".cpp";
			};
		};
	};

	public static function new_cpp_file(common_ctx, base_dir) return {
		new_source_file(common_ctx, base_dir, "src", source_file_extension(common_ctx));
	};

	public static function new_header_file(common_ctx, base_dir) return {
		new_source_file(common_ctx, base_dir, "include", ".h");
	};

	public static function new_context(common_ctx, writer, debug, file_info) return {
		{
			() with ctx_common = common_ctx;
			ctx_writer = writer;
			ctx_output = writerwrite;
			ctx_dbgout = if ( > (debug, 1)) {
				writerwrite;
			} else {
				function _: [];
			};
			ctx_calling = False;
			ctx_assigning = False;
			ctx_debug_level = debug;
			ctx_dump_src_pos = function []: [];
			ctx_return_from_block = False;
			ctx_tcall_expand_args = False;
			ctx_return_from_internal_node = False;
			ctx_real_this_ptr = True;
			ctx_real_void = False;
			ctx_dynamic_this_ptr = False;
			ctx_static_id_curr = 0;
			ctx_static_id_used = 0;
			ctx_static_id_depth = 0;
			ctx_switch_id = 0;
			ctx_class_name = "";
			ctx_class_super_name = "";
			ctx_local_function_args = Hashtbl.create(0);
			ctx_local_return_block_args = Hashtbl.create(0);
			ctx_class_member_types = Hashtbl.create(0);
			ctx_file_info = file_info;
			ctx_for_extern = False
		};
	};

	public static function new_extern_context(common_ctx, writer, debug, file_info) return {
		var ctx = new_context(common_ctx, writer, debug, file_info);
		ctx.ctx_for_extern = True;
		ctx;
	};

	public static function include_class_header(match) return switch (match) {
	case ([], @Main): False;
	case ([], Math): True;
	case path: !(is_internal_class(path));
	};

	public static function is_cpp_class(match) return switch (match) {
	case (::(cpp, _), _): True;
	case ([], EReg): True;
	case (::(haxe, []), Log): True;
	case _: False;
	};

	public static function is_scalar(typename) return {
		switch (typename) {
		case int | unsigned int | signed int | char | unsigned char | short | unsigned short | float | double | bool: True;
		case _: False;
		};
	};

	public static function is_block(exp) return {
		switch (exp.eexpr) {
		case TBlock(_): True;
		case _: False;
		};
	};

	public static function hash_keys(hash) return {
		var key_list = ref([]);
		Hashtbl.iter(function key: function value: key_list.val = ::(key, key_list.val), hash);
		key_list.val;
	};

	public static function pmap_keys(pmap) return {
		var key_list = ref([]);
		PMap.iter(function key: function _: key_list.val = ::(key, key_list.val), pmap);
		key_list.val;
	};

	public static function pmap_values(pmap) return {
		var value_list = ref([]);
		PMap.iter(function _: function value: value_list.val = ::(value, value_list.val), pmap);
		value_list.val;
	};

	public static function hash_iterate(hash, visitor) return {
		var result = ref([]);
		Hashtbl.iter(function key: function value: result.val = ::(visitor(key, value), result.val), hash);
		result.val;
	};

	public static function keyword_remap(name) return {
		switch (name) {
		case int | auto | char | const | delete | double | Float | enum | extern | float | friend | goto | long | operator | protected | register | short | signed | sizeof | template | typedef | union | unsigned | void | volatile | or | and | xor | or_eq | not | and_eq | xor_eq | typeof | stdin | stdout | stderr | system | BIG_ENDIAN | LITTLE_ENDIAN | assert | NULL | wchar_t | EOF | bool | const_cast | dynamic_cast | explicit | export | mutable | namespace | reinterpret_cast | static_cast | typeid | typename | virtual | _Complex | INFINITY | NAN | INT_MIN | INT_MAX | INT8_MIN | INT8_MAX | UINT8_MAX | INT16_MIN | INT16_MAX | UINT16_MAX | INT32_MIN | INT32_MAX | UINT32_MAX | asm | struct
					: ^ ("_hx_", name);
			case x: x;
			};
		};

		public static function remap_class_path(class_path) return {
		(new Tuple(List.map(keyword_remap, fst(class_path)), snd(class_path)));
	};

	public static function join_class_path_remap(path, separator) return {
		switch (join_class_path(remap_class_path(path), separator)) {
		case Class: "hx::Class";
		case x: x;
		};
	};

	public static function get_meta_string(meta, key) return {
		function loop(match) return switch (match) {
		case []: "";
		case ::((k, ::((Ast.EConst(Ast.String(name)), _), []), _), _) if (=(k, key)): name;
		case ::(_, l): loop(l);
		};
		loop(meta);
	};

	public static function get_meta_string_path(meta, key) return {
		function loop(match) return switch (match) {
		case []: "";
		case ::((k, ::((Ast.EConst(Ast.String(name)), _), []), pos), _) if (=(k, key)):
			try {
				if ( = (String.sub(name, 0, 2), "./")) {
					var base = if (Filename.is_relative(pos.pfile)) {
						Filename.concat(Sys.getcwd([]), pos.pfile);
					} else {
						pos.pfile;
					};
					Gencommon.normalize(Filename.concat(Filename.dirname(base), String.sub(name, 2, -(String.length(name), 2))));
				} else {
					name;
				};
			} catch (e: Invalid_argument(_)) {
				name;
			};
		case ::(_, l): loop(l);
		};
		loop(meta);
	};

	public static function get_meta_string_full_filename(meta, key) return {
		function loop(match) return switch (match) {
		case []: "";
		case ::((k, _, pos), _) if (=(k, key)):
			if (Filename.is_relative(pos.pfile)) {
				Gencommon.normalize(Filename.concat(Sys.getcwd([]), pos.pfile));
			} else {
				pos.pfile;
			};
		case ::(_, l): loop(l);
		};
		loop(meta);
	};

	public static function get_meta_string_full_dirname(meta, key) return {
		var name = get_meta_string_full_filename(meta, key);
		try {
			Gencommon.normalize(Filename.dirname(name));
		} catch (e: Invalid_argument(_)) {
			"";
		};
	};

	public static function get_field_access_meta(field_access, key) return {
		switch (field_access) {
		case FInstance(_, _, class_field) | FStatic(_, class_field): get_meta_string(class_field.cf_meta, key);
		case _: "";
		};
	};

	public static function format_code(code) return {
		String.concat("\n", ExtString.String.nsplit(code, "\r\n"));
	};

	public static function get_code(meta, key) return {
		var code = get_meta_string(meta, key);
		var magic_var = "${GENCPP_SOURCE_DIRECTORY}";
		var code = if (ExtString.String.exists(code, magic_var)) {
			var source_directory = get_meta_string_full_dirname(meta, key);
			var Tuple(_, code) = ExtString.String.replace(code, magic_var, source_directory);
			code;
		} else {
			code;
		};
		if (<>(code, "")) {
			^ (format_code(code), "\n");
		} else {
			code;
		};
	};

	public static function has_meta_key(meta, key) return {
		List.exists(function m:
		switch (m) {
	case (k, _, _) if (=(k, key)): True;
		case _: False;
		}, meta);
	};

	public static function type_has_meta_key(haxe_type, key) return {
		switch (follow(haxe_type)) {
		case TInst(klass, _): has_meta_key(klass.cl_meta, key);
		case TType(type_def, _): has_meta_key(type_def.t_meta, key);
		case TEnum(enum_def, _): has_meta_key(enum_def.e_meta, key);
		case _: False;
		};
	};

	public static function get_class_code(class_def, key) return {
		switch (class_def.cl_kind) {
		case KAbstractImpl(abstract_def): var value = get_code(abstract_def.a_meta, key);
			value;
		case _: get_code(class_def.cl_meta, key);
		};
	};

	public static function add_include(writer, class_path) return {
		writeradd_include(class_path);
	};

	public static function gen_forward_decl(writer, class_path) return {
		var output = writerwrite;
		switch (class_path) {
		case (::(@verbatim, []), file): writerwrite( ^ ("#include <", ^ (file, ">\n")));
		case _: var name = fst(remap_class_path(class_path));
			output( ^ ("HX_DECLARE_CLASS", ^ (string_of_int(List.length(name)), "[")));
			List.iter(function package_part: output( ^ (package_part, ",")), name);
			output( ^ (snd(class_path), "]\n"));
		};
	};

	public static var real_interfaces = List.filter(function (t, pl):
	switch ((new Tuple(t, pl))) {
case ( {
					cl_path = (::(cpp, ::(rtti, [])), _)
				}, []): False;
	case _: True;
	});

	public static function is_function_expr(expr) return {
		switch (expr.eexpr) {
		case TParenthesis(expr) | TMeta(_, expr): is_function_expr(expr);
		case TFunction(_): True;
		case _: False;
		};
	};

	public static function is_var_field(field) return {
		switch (field.cf_kind) {
		case Var(_): True;
		case Method(MethDynamic): True;
		case _: False;
		};
	};

	public static function has_rtti_interface(c, interface) return {
		|| (List.exists(function (t, pl): && ( = (snd(t.cl_path), interface), switch (fst(t.cl_path)) {
	case ::(cpp, ::(rtti, [])): True;
		case _: False;
		}), c.cl_implements), switch (c.cl_super) {
	case None: False;
	case Some(c, _): has_rtti_interface(c, interface);
		});
	};

	public static function has_field_integer_lookup(class_def) return {
		has_rtti_interface(class_def, "FieldIntegerLookup");
	};

	public static function has_field_integer_numeric_lookup(class_def) return {
		has_rtti_interface(class_def, "FieldNumericIntegerLookup");
	};

	public static function gen_open_namespace(output, class_path) return {
		List.iter(function namespace: output( ^ ("namespace ", ^ (namespace, "{\n"))), List.map(keyword_remap, fst(class_path)));
	};

	public static function gen_close_namespace(output, class_path) return {
		List.iter(function namespace: output( ^ ("}", ^ (" // end namespace ", ^ (namespace, "\n")))), fst(class_path));
	};

	public static function is_numeric(match) return switch (match) {
	case Int | Bool | Float | ::haxe::io::Unsigned_char__ | unsigned char: True;
	case ::cpp::UInt8 | ::cpp::Int8 | ::cpp::Char | ::cpp::UInt16 | ::cpp::Int16 | ::cpp::UInt32 | ::cpp::Int32 | ::cpp::UInt64 | ::cpp::Int64 | ::cpp::Float32 | ::cpp::Float64 | int | bool | double | float
			: True;
	case _: False;
	};

	public static function remove_parens(expression) return {
		switch (expression.eexpr) {
		case TParenthesis(e): remove_parens(e);
		case TMeta(_, e): remove_parens(e);
		case _: expression;
		};
	};

	public static function is_interface_type(t) return {
		switch (follow(t)) {
		case TInst(klass, params): klass.cl_interface;
		case _: False;
		};
	};

	public static function is_cpp_function_instance(haxe_type) return {
		switch (follow(haxe_type)) {
		case TInst(klass, params): switch (klass.cl_path) {
			case (::(cpp, []), Function): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function is_cpp_function_class(haxe_type) return {
		switch (follow(haxe_type)) {
		case TType(klass, params): switch (klass.t_path) {
			case (::(cpp, []), Function): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function is_fromStaticFunction_call(func) return {
		switch (remove_parens(func).eexpr) {
		case TField(_, FStatic({ cl_path = (::(cpp, []), Function) }, { cf_name = fromStaticFunction })): True;
		case _: False;
		};
	};

	public static function is_objc_call(field) return {
		switch (field) {
		case FStatic(cl, _) | FInstance(cl, _, _): && (cl.cl_extern, Meta.has(Meta.Objc, cl.cl_meta));
		case _: False;
		};
	};

	public static function is_objc_type(t) return {
		switch (follow(t)) {
		case TInst(cl, _): && (cl.cl_extern, Meta.has(Meta.Objc, cl.cl_meta));
		case _: False;
		};
	};

	public static function is_addressOf_call(func) return {
		switch (remove_parens(func).eexpr) {
		case TField(_, FStatic({ cl_path = (::(cpp, []), Pointer) }, { cf_name = addressOf })): True;
		case _: False;
		};
	};

	public static function is_lvalue(var) return {
		switch (remove_parens(var).eexpr) {
		case TLocal(_): True;
		case TField(_, FStatic(_, field)) | TField(_, FInstance(_, _, field)): is_var_field(field);
		case _: False;
		};
	};

	public static function is_pointer(haxe_type, includeRaw) return {
		switch (follow(haxe_type)) {
		case TInst(klass, params): switch (klass.cl_path) {
			case (::(cpp, []), Pointer) | (::(cpp, []), ConstPointer) | (::(cpp, []), Function): True;
			case (::(cpp, []), RawPointer) if (includeRaw): True;
			case (::(cpp, []), RawConstPointer) if (includeRaw): True;
			case _: False;
			};
		case TType(type_def, params): switch (type_def.t_path) {
			case (::(cpp, []), Pointer) | (::(cpp, []), ConstPointer) | (::(cpp, []), Function): True;
			case (::(cpp, []), RawPointer) if (includeRaw): True;
			case (::(cpp, []), RawConstPointer) if (includeRaw): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function is_dynamic_type_param(class_kind) return {
		switch (class_kind) {
		case KTypeParameter(_): True;
		case _: False;
		};
	};

	public static function class_string(klass, suffix, params, remap) return {
		var type_string = type_string_remap(remap);
		var join_class_path_remap = if (remap) {
			join_class_path_remap;
		} else {
			join_class_path;
		};
		switch (klass.cl_path) {
		case ([], Array) if (is_dynamic_array_param(List.hd(params))): ^ ("cpp::ArrayBase", suffix);
		case ([], Array): ^ (snd(klass.cl_path), ^ (suffix, ^ ("< ", ^ (String.concat(",", List.map(array_element_type, params)),
								 " >"))));
		case (::(cpp, []), FastIterator): ^ ("::cpp::FastIterator", ^ (suffix, ^ ("< ", ^ (String.concat(",", List.map(type_string,
												 params)), " >"))));
		case (::(cpp, []), Pointer) | (::(cpp, []), ConstPointer): ^ ("::cpp::Pointer< ", ^ (String.concat(",",
					List.map(type_string, params)), " >"));
		case (::(cpp, []), RawPointer): ^ (" ", ^ (String.concat(",", List.map(type_string, params)), " * "));
		case (::(cpp, []), RawConstPointer): ^ (" const ", ^ (String.concat(",", List.map(type_string, params)), " * "));
		case (::(cpp, []), Function): ^ ("::cpp::Function< ", ^ (cpp_function_signature_params(params), " >"));
		case _ if (is_dynamic_type_param(klass.cl_kind)): "Dynamic";
		case ([], #Int): "/* # */int";
		case (::(haxe, ::(io, [])), Unsigned_char__): "unsigned char";
		case ([], Class): "hx::Class";
		case ([], EnumValue): "Dynamic";
		case ([], Null): switch (params) {
			case ::(t, []): switch (follow(t)) {
				case TAbstract({ a_path = ([], Int) }, _) | TAbstract({ a_path = ([], Float) }, _) | TAbstract({ a_path = ([], Bool) }, _) | TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = ([], Float) }, _) | TEnum({ e_path = ([], Bool) }, _)
						: "Dynamic";
				case t if (type_has_meta_key(t, Meta.NotNull)): "Dynamic";
				case _: ^ ("/*NULL*/", type_string(t));
				};
			case _: assert False;
			};
		case path if (is_objc_type(TInst(klass, []))): var str = join_class_path_remap(klass.cl_path, "::");
			if ( = (suffix, "_obj")) {
				str;
			} else {
				if (klass.cl_interface) {
					^ ("id <", ^ (str, ">"));
				} else {
					^ (str, " *");
				};
			};
		case path if (&&(klass.cl_extern, !(is_internal_class(path)))): ^ (join_class_path_remap(klass.cl_path, "::"), suffix);
		case _: ^ ("::", ^ (join_class_path_remap(klass.cl_path, "::"), suffix));
		};
	};

	public static function type_string_suff(suffix, haxe_type, remap) return {
		var type_string = type_string_remap(remap);
		var join_class_path_remap = if (remap) {
			join_class_path_remap;
		} else {
			join_class_path;
		};
		switch (haxe_type) {
		case TMono(r): switch (r.val) {
			case None: ^ ("Dynamic", suffix);
			case Some(t): type_string_suff(suffix, t, remap);
			};
		case TAbstract({ a_path = ([], Void) }, []): "Void";
		case TAbstract({ a_path = ([], Bool) }, []): "bool";
		case TAbstract({ a_path = ([], Float) }, []): "Float";
		case TAbstract({ a_path = ([], Int) }, []): "int";
		case TAbstract({ a_path = ([], EnumValue) }, _): "Dynamic";
		case TEnum(enum, params): ^ ("::", ^ (join_class_path_remap(enum.e_path, "::"), suffix));
		case TInst(klass, params): class_string(klass, suffix, params, remap);
		case TType(type_def, params): switch (type_def.t_path) {
			case ([], Null): switch (params) {
				case ::(t, []): switch (follow(t)) {
					case TAbstract({ a_path = ([], Int) }, _) | TAbstract({ a_path = ([], Float) }, _) | TAbstract({ a_path = ([], Bool) }, _) | TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = ([], Float) }, _) | TEnum({ e_path = ([], Bool) }, _)
							: ^ ("Dynamic", suffix);
					case t if (type_has_meta_key(t, Meta.NotNull)): ^ ("Dynamic", suffix);
					case _: type_string_suff(suffix, t, remap);
					};
				case _: assert False;
				};
			case ([], Array): switch (params) {
				case ::(t, []) if (=(type_string(follow(t)), "Dynamic")): "Dynamic";
				case ::(t, []): ^ ("Array< ", ^ (type_string(follow(t)), " >"));
				case _: assert False;
				};
			case (::(cpp, []), FastIterator): switch (params) {
				case ::(t, []): ^ ("::cpp::FastIterator< ", ^ (type_string(follow(t)), " >"));
				case _: assert False;
				};
			case (::(cpp, []), Pointer) | (::(cpp, []), ConstPointer): switch (params) {
				case ::(t, []): ^ ("::cpp::Pointer< ", ^ (type_string(follow(t)), " >"));
				case _: assert False;
				};
			case (::(cpp, []), RawPointer): switch (params) {
				case ::(t, []): ^ (" ", ^ (type_string(follow(t)), " *"));
				case _: assert False;
				};
			case (::(cpp, []), RawConstPointer): switch (params) {
				case ::(t, []): ^ ("const ", ^ (type_string(follow(t)), " *"));
				case _: assert False;
				};
			case (::(cpp, []), Function): ^ ("::cpp::Function< ", ^ (cpp_function_signature_params(params), " >"));
			case _: type_string_suff(suffix, apply_params(type_def.t_params, params, type_def.t_type), remap);
			};
		case TFun(args, haxe_type): ^ ("Dynamic", suffix);
		case TAnon(a): "Dynamic";
		case TDynamic(haxe_type): ^ ("Dynamic", suffix);
		case TLazy(func): type_string_suff(suffix, func.val([]), remap);
		case TAbstract(abs, pl) if (<>(abs.a_impl, None)): type_string_suff(suffix, Abstract.get_underlying_type(abs, pl), remap);
		case TAbstract(abs, pl): ^ ("::", ^ (join_class_path_remap(abs.a_path, "::"), suffix));
		};
	};

	public static function type_string_remap(remap, haxe_type) return {
		type_string_suff("", haxe_type, remap);
	};

	public static function type_string(haxe_type) return {
		type_string_suff("", haxe_type, True);
	};

	public static function array_element_type(haxe_type) return {
		switch (type_string(haxe_type)) {
		case x if (cant_be_null(haxe_type)): x;
		case x if (is_interface_type(follow(haxe_type))): x;
		case ::String: "::String";
		case _: "::Dynamic";
		};
	};

	public static function is_dynamic_array_param(haxe_type) return {
		if ( = (type_string(follow(haxe_type)), "Dynamic")) {
			True;
		} else {
			switch (follow(haxe_type)) {
			case TInst(klass, params): switch (klass.cl_path) {
				case ([], Array) | ([], Class) | (::(cpp, []), FastIterator) | (::(cpp, []), RawPointer) | (::(cpp, []), ConstRawPointer) | (::(cpp, []), Pointer) | (::(cpp, []), ConstPointer) | (::(cpp, []), Function)
						: False;
				case _: switch (klass.cl_kind) {
					case KTypeParameter(_): True;
					case _: False;
					};
				};
			case _: False;
			};
		};
	};

	public static function cpp_function_signature(tfun, abi) return {
		switch (follow(tfun)) {
		case TFun(args, ret): ^ (type_string(ret), ^ (" ", ^ (abi, ^ ("[", ^ (gen_tfun_interface_arg_list(args), "]")))));
		case _: "void *";
		};
	};

	public static function cpp_function_signature_params(params) return {
		switch (params) {
		case ::(t, ::(abi, [])): switch (follow(abi)) {
			case TInst(klass, _): cpp_function_signature(t, get_meta_string(klass.cl_meta, Meta.Abi));
			case _: print_endline(type_string(abi));
				assert False;
			};
		case _: print_endline( ^ ("Params:", String.concat(",", List.map(type_string, params))));
			assert False;
		};
	};

	public static function gen_interface_arg_type_name(name, opt, typ) return {
		var type_str = type_string(typ);
		^ (if ( && (opt, && (cant_be_null(typ), <>(type_str, "Dynamic")))) {
		^ ("hx::Null< ", ^ (type_str, " > "));
		} else {
			type_str;
		}, ^ (" ", keyword_remap(name)));
	};

	public static function gen_tfun_interface_arg_list(args) return {
		String.concat(",", List.map(function (name, opt, typ): gen_interface_arg_type_name(name, opt, typ), args));
	};

	public static function cant_be_null(haxe_type) return {
		|| (is_numeric(type_string(haxe_type)), type_has_meta_key(haxe_type, Meta.NotNull));
	};

	public static function is_object(type_string) return {
		!( || (is_numeric(type_string), = (type_string, "::String")));
	};

	public static function is_array(haxe_type) return {
		switch (follow(haxe_type)) {
		case TInst(klass, params): switch (klass.cl_path) {
			case ([], Array): !(is_dynamic_array_param(List.hd(params)));
			case _: False;
			};
		case TType(type_def, params): switch (type_def.t_path) {
			case ([], Array): !(is_dynamic_array_param(List.hd(params)));
			case _: False;
			};
		case _: False;
		};
	};

	public static function is_array_or_dyn_array(haxe_type) return {
		switch (follow(haxe_type)) {
		case TInst(klass, params): switch (klass.cl_path) {
			case ([], Array): True;
			case _: False;
			};
		case TType(type_def, params): switch (type_def.t_path) {
			case ([], Array): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function is_array_implementer(haxe_type) return {
		switch (follow(haxe_type)) {
		case TInst(klass, params): switch (klass.cl_array_access) {
			case Some(_): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function is_numeric_field(field) return {
		switch (field.cf_kind) {
		case Var(_): is_numeric(type_string(field.cf_type));
		case _: False;
		};
	};

	public static function is_static_access(obj) return {
		switch (remove_parens(obj).eexpr) {
		case TTypeExpr(_): True;
		case _: False;
		};
	};

	public static function is_native_with_space(func) return {
		switch (remove_parens(func).eexpr) {
		case TField(obj, field) if (is_static_access(obj)): String.contains(get_field_access_meta(field, Meta.Native), ' ');
		case _: False;
		};
	};

	public static function is_cpp_function_member(func) return {
		switch (remove_parens(func).eexpr) {
		case TField(obj, field) if (is_cpp_function_instance(obj.etype)): True;
		case TCall(obj, _): is_cpp_function_member(obj);
		case _: False;
		};
	};

	public static function gen_type(ctx, haxe_type) return {
		ctx.ctx_output(type_string(haxe_type));
	};

	public static function member_type(ctx, field_object, member) return {
		var name = ^ (if (is_array(field_object.etype)) {
		"::Array";
	} else {
		type_string(field_object.etype);
		}, ^ (".", member));
		try {
			Hashtbl.find(ctx.ctx_class_member_types, name);
		} catch (e: Not_found) {
			"?";
		};
	};

	public static function is_interface(obj) return {
		is_interface_type(obj.etype);
	};

	public static function should_implement_field(x) return {
		!(is_extern_field(x));
	};

	public static function is_function_member(expression) return {
		switch (follow(expression.etype)) {
		case TFun(_, _): True;
		case _: False;
		};
	};

	public static function is_internal_member(member) return {
		switch (member) {
		case __Field | __IField | __Run | __Is | __GetClass | __GetType | __ToString | __s | __GetPtr | __SetField | __length | __IsArray | __SetThis | __Internal | __EnumParams | __Index | __Tag | __GetFields | toString | __HasField | __GetRealObject
				: True;
		case _: False;
		};
	};

	public static function is_extern_class(class_def) return {
		|| (class_def.cl_extern, || (has_meta_key(class_def.cl_meta, Meta.Extern), switch (class_def.cl_kind) {
	case KAbstractImpl(abstract_def): has_meta_key(abstract_def.a_meta, Meta.Extern);
		case _: False;
		}));
	};

	public static function is_native_gen_class(class_def) return {
		|| (has_meta_key(class_def.cl_meta, Meta.NativeGen), switch (class_def.cl_kind) {
	case KAbstractImpl(abstract_def): has_meta_key(abstract_def.a_meta, Meta.NativeGen);
		case _: False;
		});
	};

	public static function is_extern_class_instance(obj) return {
		switch (follow(obj.etype)) {
		case TInst(klass, params): klass.cl_extern;
		case _: False;
		};
	};

	public static function is_struct_access(t) return {
		switch (follow(t)) {
		case TInst(class_def, _): has_meta_key(class_def.cl_meta, Meta.StructAccess);
		case _: False;
		};
	};

	public static function is_dynamic_accessor(name, acc, field, class_def) return {
		&& ( = ( ^ (acc, ^ ("_", field.cf_name)), name), && (!(List.exists(function f: = (f.cf_name, name), class_def.cl_ordered_fields)), switch (class_def.cl_super) {
	case None: True;
	case Some(parent, _): is_dynamic_accessor(name, acc, field, parent);
		}));
	};

	public static function gen_arg_type_name(name, default_val, arg_type, prefix) return {
		var remap_name = keyword_remap(name);
		var type_str = type_string(arg_type);
		switch (default_val) {
		case Some(TNull): (new Tuple(type_str, remap_name));
		case Some(constant) if (cant_be_null(arg_type)): (new Tuple( ^ ("hx::Null< ", ^ (type_str, " > ")), ^ (prefix,
			remap_name)));
		case Some(constant): (new Tuple(type_str, ^ (prefix, remap_name)));
		case _: (new Tuple(type_str, remap_name));
		};
	};

	public static function gen_arg(name, default_val, arg_type, prefix) return {
		var pair = gen_arg_type_name(name, default_val, arg_type, prefix);
		^ (fst(pair), ^ (" ", snd(pair)));
	};

	public static function gen_arg_list(arg_list, prefix) return {
		String.concat(",", List.map(function (v, o): gen_arg(v.v_name, o, v.v_type, prefix), arg_list));
	};

	public static function gen_tfun_arg_list(arg_list) return {
		switch (arg_list) {
		case []: "";
		case ::((name, o, arg_type), []): gen_arg(name, None, arg_type, "");
		case ::((name, o, arg_type), remaining): ^ (gen_arg(name, None, arg_type, ""), ^ (",", gen_tfun_arg_list(remaining)));
		};
	};

	public static function implement_dynamic_here(class_def) return {
		function implements_dynamic(c) return {
			switch (c.cl_dynamic) {
			case None: False;
			case _: True;
			};
		};
		function super_implements_dynamic(c) return {
			switch (c.cl_super) {
			case None: False;
			case Some(csup, _): if (implements_dynamic(csup)) {
					True;
				} else {
					super_implements_dynamic(csup);
				};
			};
		};
		&& (implements_dynamic(class_def), !(super_implements_dynamic(class_def)));
	};

	public static function gen_hash32(seed, str) return {
		var h = ref(Int32.of_int(seed));
		var cycle = Int32.of_int(223);
		for (i in /*to*/0... - (String.length(str), 1)) {
			h.val = Int32.add(Int32.mul(h.val, cycle), Int32.of_int(int_of_char(String.unsafe_get(str, i))));
		};
		h.val;
	};

	public static function gen_hash(seed, str) return {
		Printf.sprintf("0x%08lx", gen_hash32(seed, str));
	};

	public static function gen_string_hash(str) return {
		var h = gen_hash32(0, str);
		Printf.sprintf("\"\\x%02lx\",\"\\x%02lx\",\"\\x%02lx\",\"\\x%02lx\"", Int32.shift_right_logical(Int32.shift_left(h, 24), 24), Int32.shift_right_logical(Int32.shift_left(h, 16), 24), Int32.shift_right_logical(Int32.shift_left(h, 8), 24), Int32.shift_right_logical(h, 24));
	};

	public static function escape_stringw(s, l) return {
		var b = Buffer.create(0);
		Buffer.add_char(b, 'L');
		Buffer.add_char(b, '"');
		var skip = ref(0);
		for (i in /*to*/0... - (String.length(s), 1)) {
			if ( > (skip.val, 0)) {
				skip.val = -(skip.val, 1);
				l.val = -(l.val, 1);
			} else {
				switch (Char.code(String.unsafe_get(s, i))) {
				case c if (>(c, 127)): var encoded = lor(lsl(land(c, 0x3F), 6), land(Char.code(String.unsafe_get(s, +(i, 1))), 0x7F));
					skip.val = 1;
					Buffer.add_string(b, Printf.sprintf("\\x%X\"L\"", encoded));
				case c if (<(c, 32)): Buffer.add_string(b, Printf.sprintf("\\x%X\"L\"", c));
				case c: Buffer.add_char(b, Char.chr(c));
				};
			};
		};
		Buffer.add_char(b, '"');
		Buffer.contents(b);
	};

	public static function special_to_hex(s) return {
		var l = String.length(s);
		var b = Buffer.create(0);
		for (i in /*to*/0... - (l, 1)) {
			switch (Char.code(String.unsafe_get(s, i))) {
			case c if (||(>(c, 127), <(c, 32))): Buffer.add_string(b, Printf.sprintf("\\x%02x\"\"", c));
			case c: Buffer.add_char(b, Char.chr(c));
			};
		};
		Buffer.contents(b);
	};

	public static function escape_extern(s) return {
		var l = String.length(s);
		var b = Buffer.create(0);
		for (i in /*to*/0... - (l, 1)) {
			switch (Char.code(String.unsafe_get(s, i))) {
			case c if (||(>(c, 127), ||(<(c, 32), ||(=(c, 34), =(c, 92))))): Buffer.add_string(b, Printf.sprintf("\\x%02x", c));
			case c: Buffer.add_char(b, Char.chr(c));
			};
		};
		Buffer.contents(b);
	};

	public static function has_utf8_chars(s) return {
		var result = ref(False);
		for (i in /*to*/0... - (String.length(s), 1)) {
			result.val = || (result.val, > (Char.code(String.unsafe_get(s, i)), 127));
		};
		result.val;
	};

	public static function escape_command(s) return {
		var b = Buffer.create(0);
		String.iter(function ch:
		if ( || ( == (ch, '"'), == (ch, '\\'))) {
		Buffer.add_string(b, "\\");
		} else {
			[];
		};
		Buffer.add_char(b, ch), s);
		Buffer.contents(b);
	};

	public static function str(s) return {
		function split(s, plus) return {
			var escaped = Ast.s_escape(hex = False, s);
			var hexed = special_to_hex(escaped);
			if ( <= (String.length(hexed), 16000)) {
				^ (plus, ^ (" HX_CSTRING[\"", ^ (hexed, "\"]")));
			} else {
				var len = String.length(s);
				var half = lsr(len, 1);
				^ (split(String.sub(s, 0, half), plus), split(String.sub(s, half, -(len, half)), "+"));
			};
		};
		var escaped = Ast.s_escape(hex = False, s);
		var hexed = special_to_hex(escaped);
		if ( <= (String.length(hexed), 16000)) {
			^ ("HX_HCSTRING[\"", ^ (hexed, ^ ("\",", ^ (gen_string_hash(s), "]"))));
		} else {
			^ ("[", ^ (split(s, ""), "]"));
		};
	};

	public static function const_char_star(s) return {
		var escaped = Ast.s_escape(hex = False, s);
		^ ("\"", ^ (special_to_hex(escaped), "\""));
	};

	public static function clear_real_this_ptr(ctx, dynamic_this) return {
		var old_flag = ctx.ctx_real_this_ptr;
		var old_dynamic = ctx.ctx_dynamic_this_ptr;
		var old_void = ctx.ctx_real_void;
		ctx.ctx_real_this_ptr = False;
		ctx.ctx_dynamic_this_ptr = dynamic_this;
		function []: ctx.ctx_real_this_ptr = old_flag;
		ctx.ctx_dynamic_this_ptr = old_dynamic;
		ctx.ctx_real_void = old_void;
	};

	public static function next_anon_function_name(ctx) return {
		ctx.ctx_static_id_curr = +(ctx.ctx_static_id_curr, 1);
		^ ("_Function_", ^ (string_of_int(ctx.ctx_static_id_depth), ^ ("_", string_of_int(ctx.ctx_static_id_curr))));
	};

	public static function use_anon_function_name(ctx) return {
		ctx.ctx_static_id_used = +(ctx.ctx_static_id_used, 1);
		^ ("_Function_", ^ (string_of_int(ctx.ctx_static_id_depth), ^ ("_", string_of_int(ctx.ctx_static_id_used))));
	};

	public static function push_anon_names(ctx) return {
		var old_used = ctx.ctx_static_id_used;
		var old_curr = ctx.ctx_static_id_curr;
		var old_depth = ctx.ctx_static_id_depth;
		ctx.ctx_static_id_used = 0;
		ctx.ctx_static_id_curr = 0;
		ctx.ctx_static_id_depth = +(ctx.ctx_static_id_depth, 1);
		function []: ctx.ctx_static_id_used = old_used;
		ctx.ctx_static_id_curr = old_curr;
		ctx.ctx_static_id_depth = old_depth;
	};

	public static function get_switch_var(ctx) return {
		ctx.ctx_switch_id = +(ctx.ctx_switch_id, 1);
		^ ("_switch_", string_of_int(ctx.ctx_switch_id));
	};

	public static function debug_expression(expression, type_too) return {
		^ ("/* ", ^ (Type.s_expr_kind(expression), ^ (if (type_too) {
		^ (" = ", type_string(expression.etype));
		} else {
			"";
		}, " */")));
	};

	public static function iter_retval(f, retval, e) return {
		switch (e.eexpr) {
		case TConst(_) | TLocal(_) | TBreak | TContinue | TTypeExpr(_): [];
		case TArray(e1, e2) | TBinop(_, e1, e2): f(True, e1);
			f(True, e2);
		case TWhile(e1, e2, _): f(True, e1);
			f(False, e2);
		case TFor(_, e1, e2): f(True, e1);
			f(False, e2);
		case TThrow(e) | TField(e, _) | TEnumParameter(e, _, _) | TUnop(_, _, e): f(True, e);
		case TParenthesis(e) | TMeta(_, e): f(retval, e);
		case TBlock(expr_list) if (retval): function return_last(match) return switch (match) {
			case []: [];
			case ::(expr, []): f(True, expr);
			case ::(expr, exprs): f(False, expr);
				return_last(exprs);
			};
			return_last(expr_list);
		case TArrayDecl(el) | TNew(_, _, el): List.iter(f(True), el);
		case TBlock(el): List.iter(f(False), el);
		case TObjectDecl(fl): List.iter(function (_, e): f(True, e), fl);
		case TCall(e, el): f(True, e);
			List.iter(f(True), el);
		case TVar(_, eo): switch (eo) {
			case None: [];
			case Some(e): f(True, e);
			};
		case TFunction(fu): f(False, fu.tf_expr);
		case TIf(e, e1, e2): f(True, e);
			f(retval, e1);
			switch (e2) {
			case None: [];
			case Some(e): f(retval, e);
			};
		case TSwitch(e, cases, def): f(True, e);
			List.iter(function (el, e2): List.iter(f(True), el);
					  f(retval, e2), cases);
			switch (def) {
			case None: [];
			case Some(e): f(retval, e);
			};
		case TTry(e, catches): f(retval, e);
			List.iter(function (_, e): f(False, e), catches);
		case TReturn(eo): switch (eo) {
			case None: [];
			case Some(e): f(True, e);
			};
		case TCast(e, None): f(retval, e);
		case TCast(e, _): f(True, e);
		};
	};

	public static function array_arg_list(inList) return {
		var i = ref(-(0, 1));
		String.concat(",", List.map(function _: incr(i);
		^ ("inArgs[", ^ (string_of_int(i.val), "]")), inList));
	};

	public static function list_num(l) return {
		string_of_int(List.length(l));
	};

	public static function only_int_cases(cases) return {
		switch (cases) {
		case []: False;
		case _: !(List.exists(function (cases, expression): List.exists(function case: switch (case.eexpr) {
			case TConst(TInt(_)): False;
				case _: True;
				}, cases), cases));
		};
	};

	public static function contains_break(expression) return {
		try {
			function check_all(expression) return {
				Type.iter(function expr:
				switch (expr.eexpr) {
			case TBreak: raise(BreakFound);
				case TFor(_) | TFunction(_) | TWhile(_, _, _): [];
				case _: check_all(expr);
				}, expression);
			};
			check_all(expression);
			False;
		} catch (e: BreakFound) {
			True;
		};
	};

	public static function dynamic_internal(match) return switch (match) {
	case __Is: True;
	case _: False;
	};

	public static function is_null(expr) return {
		switch (expr.eexpr) {
		case TConst(TNull): True;
		case TParenthesis(expr) | TMeta(_, expr): is_null(expr);
		case TCast(e, None): is_null(e);
		case _: False;
		};
	};

	public static function find_undeclared_variables_ctx(ctx, undeclared, declarations, this_suffix, allow_this,
			expression) return {
		var output = ctx.ctx_output;
		function find_undeclared_variables(undeclared, declarations, this_suffix, allow_this, expression) return {
			switch (expression.eexpr) {
			case TVar(tvar, optional_init): Hashtbl.add(declarations, keyword_remap(tvar.v_name), []);
				if ( > (ctx.ctx_debug_level, 1)) {
					output( ^ ("/* found var ", ^ (tvar.v_name, "*/ ")));
				} else {
					[];
				};
				switch (optional_init) {
				case Some(expression): find_undeclared_variables(undeclared, declarations, this_suffix, allow_this, expression);
				case _: [];
				};
			case TFunction(func): List.iter(function (tvar, opt_val): if ( > (ctx.ctx_debug_level, 1)) {
				output( ^ ("/* found arg ", ^ (tvar.v_name, ^ (" = ", ^ (type_string(tvar.v_type), " */ ")))));
				} else {
					[];
				};
				Hashtbl.add(declarations, keyword_remap(tvar.v_name), []), func.tf_args);
				find_undeclared_variables(undeclared, declarations, this_suffix, False, func.tf_expr);
			case TTry(try_block, catches): find_undeclared_variables(undeclared, declarations, this_suffix, allow_this, try_block);
				List.iter(function (tvar, catch_expt): var old_decs = Hashtbl.copy(declarations);
						  Hashtbl.add(declarations, keyword_remap(tvar.v_name), []);
						  find_undeclared_variables(undeclared, declarations, this_suffix, allow_this, catch_expt);
						  Hashtbl.clear(declarations);
						  Hashtbl.iter(Hashtbl.add(declarations), old_decs), catches);
			case TLocal(tvar): var name = keyword_remap(tvar.v_name);
				if (!(Hashtbl.mem(declarations, name))) {
					Hashtbl.replace(undeclared, name, type_string(expression.etype));
				} else {
					[];
				};
			case TFor(tvar, init, loop): var old_decs = Hashtbl.copy(declarations);
				Hashtbl.add(declarations, keyword_remap(tvar.v_name), []);
				find_undeclared_variables(undeclared, declarations, this_suffix, allow_this, init);
				find_undeclared_variables(undeclared, declarations, this_suffix, allow_this, loop);
				Hashtbl.clear(declarations);
				Hashtbl.iter(Hashtbl.add(declarations), old_decs);
			case TConst(TSuper) | TConst(TThis): if ( && (!(Hashtbl.mem(declarations, "this")), allow_this)) {
					Hashtbl.replace(undeclared, "this", type_string_suff(this_suffix, expression.etype, True));
				} else {
					[];
				};
			case TBlock(expr_list): var old_decs = Hashtbl.copy(declarations);
				List.iter(find_undeclared_variables(undeclared, declarations, this_suffix, allow_this), expr_list);
				Hashtbl.clear(declarations);
				Hashtbl.iter(Hashtbl.add(declarations), old_decs);
			case _: Type.iter(find_undeclared_variables(undeclared, declarations, this_suffix, allow_this), expression);
			};
		};
		find_undeclared_variables(undeclared, declarations, this_suffix, allow_this, expression);
	};

	public static function is_dynamic_in_cpp(ctx, expr) return {
		var expr_type = type_string(switch (follow(expr.etype)) {
	case TFun(args, ret): ret;
		case _: expr.etype;
		});
		ctx.ctx_dbgout( ^ ("/* idic: ", ^ (expr_type, " */")));
		if ( || ( = (expr_type, "Dynamic"), = (expr_type, "cpp::ArrayBase"))) {
			True;
		} else {
			var result = switch (expr.eexpr) {
			case TEnumParameter(obj, _, index): True;
			case TField(obj, field): var name = field_name(field);
				ctx.ctx_dbgout( ^ ("/* ?tfield ", ^ (name, " */")));
				if (is_dynamic_member_lookup_in_cpp(ctx, obj, field)) {
					ctx.ctx_dbgout("/* tf=dynobj */");
					True;
				} else {
					if (is_dynamic_member_return_in_cpp(ctx, obj, field)) {
						ctx.ctx_dbgout("/* tf=dynret */");
						True;
					} else {
						ctx.ctx_dbgout("/* tf=notdyn */");
						False;
					};
				};
			case TConst(TThis) if (&&(!(ctx.ctx_real_this_ptr), ctx.ctx_dynamic_this_ptr)): ctx.ctx_dbgout("/* dthis */");
				True;
			case TArray(obj, index): var dyn = is_dynamic_in_cpp(ctx, obj);
				ctx.ctx_dbgout( ^ ("/* aidr:", ^ (if (dyn) {
				"Dyn";
			} else {
				"Not";
			}, " */")));
				dyn;
			case TTypeExpr(_): False;
			case TCall(func, args): switch (follow(func.etype)) {
				case TFun(args, ret): ctx.ctx_dbgout( ^ ("/* ret = ", ^ (type_string(ret), " */")));
					is_dynamic_in_cpp(ctx, func);
				case _: ctx.ctx_dbgout("/* not TFun */");
					True;
				};
			case TParenthesis(expr) | TMeta(_, expr): is_dynamic_in_cpp(ctx, expr);
			case TCast(e, None): = (type_string(expr.etype), "Dynamic");
			case TLocal({ v_name = __global__ }): False;
			case TConst(TNull): True;
			case _: ctx.ctx_dbgout("/* other */");
				False;
			};
			ctx.ctx_dbgout(if (result) {
			"/* Y */";
		} else {
			"/* N */";
		});
			result;
		};
	};

	public static function is_dynamic_member_lookup_in_cpp(ctx, field_object, field) return {
		var member = field_name(field);
		ctx.ctx_dbgout( ^ ("/*mem.", ^ (member, ".*/")));
		if (is_internal_member(member)) {
			False;
		} else {
			if (is_pointer(field_object.etype, True)) {
				False;
			} else {
				if (switch (field_object.eexpr) {
				case TTypeExpr(_): ctx.ctx_dbgout("/*!TTypeExpr*/");
						True;
					case _: False;
					}) {
					False;
				} else {
					if (is_dynamic_in_cpp(ctx, field_object)) {
						True;
					} else {
						if (is_array(field_object.etype)) {
							False;
						} else {
							var tstr = type_string(field_object.etype);
							ctx.ctx_dbgout( ^ ("/* ts:", ^ (tstr, "*/")));
							switch (tstr) {
							case ::String | Null | ::hx::Class | ::Enum | ::Math | ::ArrayAccess: ctx.ctx_dbgout( ^ ("/* ok:",
								^ (type_string(field_object.etype), " */")));
								False;
							case Dynamic: True;
							case name: var full_name = ^ (name, ^ (".", member));
								ctx.ctx_dbgout( ^ ("/* t:", ^ (full_name, " */")));
								try {
									var mem_type = Hashtbl.find(ctx.ctx_class_member_types, full_name);
									ctx.ctx_dbgout( ^ ("/* =", ^ (mem_type, "*/")));
									False;
								} catch (e: Not_found) {
									!(is_extern_class_instance(field_object));
								};
							};
						};
					};
				};
			};
		};
	};

	public static function is_dynamic_member_return_in_cpp(ctx, field_object, field) return {
		var member = field_name(field);
		if (is_array(field_object.etype)) {
			False;
		} else {
			if (is_pointer(field_object.etype, True)) {
				False;
			} else {
				if (is_internal_member(member)) {
					False;
				} else {
					switch (field_object.eexpr) {
					case TTypeExpr(t): var full_name = ^ ("::", ^ (join_class_path(t_path(t), "::"), ^ (".", member)));
						ctx.ctx_dbgout( ^ ("/*static:", ^ (full_name, "*/")));
						try {
							var mem_type = Hashtbl.find(ctx.ctx_class_member_types, full_name);
							|| ( = (mem_type, "Dynamic"), = (mem_type, "cpp::ArrayBase"));
						} catch (e: Not_found) {
							True;
						};
					case _: var tstr = type_string(field_object.etype);
						switch (tstr) {
						case ::String | Null | ::hx::Class | ::Enum | ::Math | ::ArrayAccess: False;
						case Dynamic | cpp::ArrayBase: ctx.ctx_dbgout("/*D*/");
							True;
						case name: var full_name = ^ (name, ^ (".", member));
							ctx.ctx_dbgout( ^ ("/*R:", ^ (full_name, "*/")));
							try {
								var mem_type = Hashtbl.find(ctx.ctx_class_member_types, full_name);
								|| ( = (mem_type, "Dynamic"), = (mem_type, "cpp::ArrayBase"));
							} catch (e: Not_found) {
								True;
							};
						};
					};
				};
			};
		};
	};

	public static function cast_if_required(ctx, expr, to_type) return {
		var expr_type = type_string(expr.etype);
		ctx.ctx_dbgout( ^ ("/* cir: ", ^ (expr_type, " */")));
		if (is_dynamic_in_cpp(ctx, expr)) {
			ctx.ctx_output( ^ (".Cast< ", ^ (to_type, " >[]")));
		} else {
			[];
		};
	};

	public static function is_matching_interface_type(t0, t1) return {
		switch ((new Tuple(follow(t0), follow(t1)))) {
		case (TInst(k0, _), TInst(k1, _)): == (k0, k1);
		case _: False;
		};
	};

	public static function default_value_string(match) return switch (match) {
	case TInt(i): Printf.sprintf("%ld", i);
	case TFloat(float_as_string): ^ ("[[Float]", ^ (float_as_string, "]"));
	case TString(s): str(s);
	case TBool(b): if (b) {
			"true";
		} else {
			"false";
		};
	case TNull: "null[]";
	case _: "/* Hmmm */";
	};

	public static function generate_default_values(ctx, args, prefix) return {
		List.iter(function (v, o): var type_str = type_string(v.v_type);
		var name = keyword_remap(v.v_name);
		switch (o) {
	case Some(TNull): [];
		case Some(const): ctx.ctx_output( ^ (type_str, ^ (" ", ^ (name, ^ (" = ", ^ (prefix, ^ (name, ^ (".Default[",
			^ (default_value_string(const), "];\n")))))))));
		case _: [];
		}, args);
	};

	public static function return_type_string(t) return {
		switch (t) {
		case TFun(_, ret): type_string(ret);
		case _: "";
		};
	};

	public static function get_return_type(field) return {
		switch (follow(field.cf_type)) {
		case TFun(_, return_type): return_type;
		case _: raise(Not_found);
		};
	};

	public static function has_default_values(args) return {
		List.exists(function (_, o):
		switch (o) {
	case Some(TNull): False;
		case Some(_): True;
		case _: False;
		}, args);
	};

	public static function strip_file(ctx, file) return {
		switch (Common.defined(ctx, Common.Define.AbsolutePath)) {
		case True: file;
		case False: var flen = String.length(file);
			try {
				List.iter(function path: var plen = String.length(path);
				if ( && ( > (flen, plen), = (path, String.sub(file, 0, plen)))) {
				raise(PathFound(String.sub(file, plen, -(flen, plen))));
				} else {
					[];
				}, @(ctx.class_path, ctx.std_path));
				file;
			} catch (e: PathFound(tail)) {
				tail;
			};
		};
	};

	public static function hx_stack_push(ctx, output, clazz, func_name, pos) return {
		if ( > (ctx.ctx_debug_level, 0)) {
			var stripped_file = strip_file(ctx.ctx_common, pos.pfile);
			var qfile = ^ ("\"", ^ (Ast.s_escape(stripped_file), "\""));
			ctx.ctx_file_info.val = PMap.add(stripped_file, pos.pfile, ctx.ctx_file_info.val);
			if ( > (ctx.ctx_debug_level, 0)) {
				var hash_class_func = gen_hash(0, ^ (clazz, ^ (".", func_name)));
				var hash_file = gen_hash(0, stripped_file);
				output( ^ ("HX_STACK_FRAME[\"", ^ (clazz, ^ ("\",\"", ^ (func_name, ^ ("\",", ^ (hash_class_func, ^ (",\"", ^ (clazz,
				^ (".", ^ (func_name, ^ ("\",", ^ (qfile, ^ (",", ^ (string_of_int(Lexer.get_error_line(pos)), ^ (",", ^ (hash_file,
				"]\n")))))))))))))))));
			} else {
				[];
			};
		} else {
			[];
		};
	};

	public static function gen_expression_tree(ctx, retval, expression_tree, set_var, tail_code) return {
		var writer = ctx.ctx_writer;
		var output_i = writerwrite_i;
		var output = ctx.ctx_output;
		function define_local_function_ctx(func_name, func_def) return {
			function remap_this(match) return switch (match) {
			case this: "__this";
			case other: other;
			};
			function define_local_function(func_name, func_def) return {
				var declarations = Hashtbl.create(0);
				var undeclared = Hashtbl.create(0);
				Hashtbl.add(declarations, "__global__", []);
				Hashtbl.add(declarations, "__cpp__", []);
				Hashtbl.add(declarations, "__trace", []);
				List.iter(function (arg_var, opt_val):
				if ( > (ctx.ctx_debug_level, 1)) {
				output( ^ ("/* found arg ", ^ (arg_var.v_name, ^ (" = ", ^ (type_string(arg_var.v_type), " */ ")))));
				} else {
					[];
				};
				Hashtbl.add(declarations, keyword_remap(arg_var.v_name), []), func_def.tf_args);
				find_undeclared_variables_ctx(ctx, undeclared, declarations, "", True, func_def.tf_expr);
				var has_this = Hashtbl.mem(undeclared, "this");
				if (has_this) {
					Hashtbl.remove(undeclared, "this");
				} else {
					[];
				};
				var typed_vars = hash_iterate(undeclared, function key: function value: ^ (value, ^ (",", keyword_remap(key))));
				var func_name_sep = ^ (func_name, if ( > (List.length(typed_vars), 0)) {
				",";
			} else {
				"";
			});
				output_i( ^ ("HX_BEGIN_LOCAL_FUNC_S", ^ (list_num(typed_vars), ^ ("[", ^ (if (has_this) {
				"hx::LocalThisFunc,";
			} else {
				"hx::LocalFunc,";
			}, ^ (func_name_sep, ^ (String.concat(",", typed_vars), "]\n")))))));
				output_i( ^ ("int __ArgCount[] const { return ", ^ (string_of_int(List.length(func_def.tf_args)), "; }\n")));
				var args_and_types = List.map(function (v, _): ^ (type_string(v.v_type), ^ (" ", keyword_remap(v.v_name))), func_def.tf_args);
				var block = is_block(func_def.tf_expr);
				var func_type = type_string(func_def.tf_type);
				output_i( ^ (func_type, ^ (" run[", ^ (gen_arg_list(func_def.tf_args, "__o_"), "]"))));
				var close_defaults = if (has_default_values(func_def.tf_args)) {
					writerbegin_block;
					output_i("");
					generate_default_values(ctx, func_def.tf_args, "__o_");
					output_i("");
					True;
				} else {
					False;
				};
				var pop_real_this_ptr = clear_real_this_ptr(ctx, True);
				writerbegin_block;
				if ( > (ctx.ctx_debug_level, 0)) {
					hx_stack_push(ctx, output_i, "*", func_name, func_def.tf_expr.epos);
					if ( && (has_this, > (ctx.ctx_debug_level, 0))) {
						output_i("HX_STACK_THIS[__this.mPtr]\n");
					} else {
						[];
					};
					List.iter(function (v, _): output_i( ^ ("HX_STACK_ARG[", ^ (keyword_remap(v.v_name), ^ (",\"", ^ (v.v_name, "\"]\n"))))),
							  func_def.tf_args);
				} else {
					[];
				};
				if (block) {
					output_i("");
					gen_expression(False, func_def.tf_expr);
					output_i("return null[];\n");
				} else {
					var pop_names = push_anon_names(ctx);
					find_local_functions_and_return_blocks_ctx(False, func_def.tf_expr);
					switch (func_def.tf_expr.eexpr) {
					case TReturn(Some(return_expression)) if (<>(func_type, "Void")): output_i("return ");
						gen_expression(True, return_expression);
					case TReturn(Some(return_expression)): output_i("");
						gen_expression(False, return_expression);
					case _: output_i("");
						gen_block_expression(func_def.tf_expr);
					};
					output(";\n");
					output_i("return null[];\n");
					pop_names([]);
				};
				writerend_block;
				if (close_defaults) {
					writerend_block;
				} else {
					[];
				};
				pop_real_this_ptr([]);
				var return = if ( = (type_string(func_def.tf_type), "Void")) {
					"[void]";
				} else {
					"return";
				};
				output_i( ^ ("HX_END_LOCAL_FUNC", ^ (list_num(args_and_types), ^ ("[", ^ (return, "]\n\n")))));
				Hashtbl.replace(ctx.ctx_local_function_args, func_name, if (ctx.ctx_real_this_ptr) {
				String.concat(",", hash_keys(undeclared));
				} else {
					String.concat(",", List.map(remap_this, hash_keys(undeclared)));
				});
			};
			define_local_function(func_name, func_def);
		};
		function find_local_functions_and_return_blocks_ctx(retval, expression) return {
			function find_local_functions_and_return_blocks(retval, expression) return {
				switch (expression.eexpr) {
				case TBlock(_): if (retval) {
						define_local_return_block_ctx(expression, next_anon_function_name(ctx), True);
					} else {
						[];
					};
				case TTry(_, _) | TSwitch(_, _, _) if (retval): define_local_return_block_ctx(expression, next_anon_function_name(ctx),
							True);
				case TObjectDecl(::((fileName, { eexpr = TConst(TString(file)) }), ::((lineNumber, { eexpr = TConst(TInt(line)) }), ::((className, { eexpr = TConst(TString(class_name)) }), ::((methodName, { eexpr = TConst(TString(meth)) }), [])))))
						: [];
				case TObjectDecl(decl_list): var name = next_anon_function_name(ctx);
					define_local_return_block_ctx(expression, name, True);
				case TFunction(func): var func_name = next_anon_function_name(ctx);
					output("\n");
					define_local_function_ctx(func_name, func);
				case TField(obj, _) | TEnumParameter(obj, _, _) if (is_null(obj)): [];
				case TArray(obj, _) if (is_null(obj)): [];
				case TIf(_, _, _) if (retval): iter_retval(find_local_functions_and_return_blocks, retval, expression);
				case TSwitch(_, _, _) if (retval): [];
				case TWhile(cond, _, _) | TIf(cond, _, _) | TSwitch(cond, _, _): iter_retval(find_local_functions_and_return_blocks, True,
							cond);
				case _: iter_retval(find_local_functions_and_return_blocks, retval, expression);
				};
			};
			find_local_functions_and_return_blocks(retval, expression);
		};
		function define_local_return_block_ctx(expression, name, retval) return {
			function check_this(match) return switch (match) {
			case this if (!(ctx.ctx_real_this_ptr)): "__this";
			case x: x;
			};
			function define_local_return_block(expression) return {
				var declarations = Hashtbl.create(0);
				var undeclared = Hashtbl.create(0);
				Hashtbl.add(declarations, "__global__", []);
				Hashtbl.add(declarations, "__cpp__", []);
				Hashtbl.add(declarations, "__trace", []);
				find_undeclared_variables_ctx(ctx, undeclared, declarations, "_obj", True, expression);
				var vars = hash_keys(undeclared);
				var args = String.concat(",", List.map(check_this, hash_keys(undeclared)));
				Hashtbl.replace(ctx.ctx_local_return_block_args, name, args);
				output_i( ^ ("struct ", name));
				writerbegin_block;
				var ret_type = if (!(retval)) {
					"Void";
				} else {
					switch (expression.eexpr) {
					case TObjectDecl(_): "Dynamic";
					case _: type_string(expression.etype);
					};
				};
				function pass_by_value(name) return {
					&& ( >= (String.length(name), 5), = (String.sub(name, 0, 5), "_this"));
				};
				output_i( ^ ("inline static ", ^ (ret_type, " Block[ ")));
				output(String.concat(",", List.map(function var: var var_type = Hashtbl.find(undeclared, var);
				switch (var) {
			case this: ^ ("hx::ObjectPtr< ", ^ (var_type, " > __this"));
				case name if (pass_by_value(name)): ^ (var_type, ^ (" ", name));
				case name: ^ (var_type, ^ (" &", name));
				}, vars)));
				output("]");
				var return_data = <>(ret_type, "Void");
				writerbegin_block;
				hx_stack_push(ctx, output_i, "*", "closure", expression.epos);
				output_i("");
				var pop_real_this_ptr = clear_real_this_ptr(ctx, False);
				switch (expression.eexpr) {
				case TObjectDecl(decl_list): writerbegin_block;
					output_i("hx::Anon __result = hx::Anon_obj::Create[];\n");
					var pop_names = push_anon_names(ctx);
					List.iter(function (name, value): find_local_functions_and_return_blocks_ctx(True, value);
							  output_i( ^ ("__result->Add[", ^ (str(name), " , ")));
							  gen_expression(True, value);
					output(if (is_function_expr(value)) {
					",true";
				} else {
					",false";
				});
				output("];\n"), decl_list);
					pop_names([]);
					output_i("return __result;\n");
					writerend_block;
				case TBlock(_): ctx.ctx_return_from_block = return_data;
					ctx.ctx_return_from_internal_node = False;
					gen_expression(False, expression);
				case TCall(func, args): writerbegin_block;
					var pop_names = push_anon_names(ctx);
					find_local_functions_and_return_blocks_ctx(True, func);
					List.iter(find_local_functions_and_return_blocks_ctx(True), args);
					ctx.ctx_tcall_expand_args = True;
					gen_expression(return_data, expression);
					output(";\n");
					pop_names([]);
					writerend_block;
				case _: ctx.ctx_return_from_block = False;
					ctx.ctx_return_from_internal_node = return_data;
					gen_block_expression(expression);
				};
				output_i("return null[];\n");
				writerend_block;
				pop_real_this_ptr([]);
				writerend_block_line;
				output(";\n");
			};
			define_local_return_block(expression);
		};
		function gen_expression(retval, expression) return {
			var calling = ctx.ctx_calling;
			ctx.ctx_calling = False;
			var assigning = ctx.ctx_assigning;
			ctx.ctx_assigning = False;
			var return_from_block = ctx.ctx_return_from_block;
			ctx.ctx_return_from_block = False;
			var tcall_expand_args = ctx.ctx_tcall_expand_args;
			ctx.ctx_tcall_expand_args = False;
			var return_from_internal_node = ctx.ctx_return_from_internal_node;
			ctx.ctx_return_from_internal_node = False;
			var dump_src_pos = ctx.ctx_dump_src_pos;
			ctx.ctx_dump_src_pos = function []: [];
			if ( > (ctx.ctx_debug_level, 1)) {
				output(debug_expression(expression, > (ctx.ctx_debug_level, 1)));
			} else {
				[];
			};
			function gen_expression_list(expressions) return {
				switch (expressions) {
				case []: [];
				case ::(single, []): gen_expression(True, single);
				case ::(first, remaining): gen_expression(True, first);
					output(",");
					gen_expression_list(remaining);
				};
			};
			function check_objc_unbox(expression, to_type) return {
				if ( && (is_objc_type(to_type), !(is_objc_type(expression.etype)))) {
					{
						(expression) with eexpr = TCast(expression, None);
						etype = to_type
					};
				} else {
					expression;
				};
			};
			function check_objc_box(expression, to_type) return {
				if ( && (is_objc_type(expression.etype), !(is_objc_type(to_type)))) {
					{
						(expression) with eexpr = TCast(expression, None);
						etype = to_type
					};
				} else {
					expression;
				};
			};
			function add_objc_cast_if_needed(expression) return {
				var is_cast = && (retval, && (is_objc_type(expression.etype), is_dynamic_in_cpp(ctx, expression)));
				if (is_cast) {
					output( ^ ("[ [", ^ (type_string(expression.etype), "] [id] [")));
					"] ]";
				} else {
					"";
				};
			};
			function gen_bin_op_string(expr1, op, expr2) return {
				var cast = switch (op) {
				case > > | < < | & | | | ^: "int[";
				case && | ||: "bool[";
				case /: "Float[";
				case _: "";
				};
				if (<>(op, "=")) {
					output("[");
				} else {
					[];
				};
				if (<>(cast, "")) {
					output(cast);
				} else {
					[];
				};
				gen_expression(True, expr1);
				if (<>(cast, "")) {
					output("]");
				} else {
					[];
				};
				output( ^ (" ", ^ (op, " ")));
				if (<>(cast, "")) {
					output(cast);
				} else {
					[];
				};
				gen_expression(True, expr2);
				if (<>(cast, "")) {
					output("]");
				} else {
					[];
				};
				if (<>(op, "=")) {
					output("]");
				} else {
					[];
				};
			};
			function is_const_string_term(expr) return {
				switch (expr.eexpr) {
				case TConst(TString(_)): True;
				case TBinop(OpAdd, e1, e2): && (is_const_string_term(e1), is_const_string_term(e2));
				case _: False;
				};
			};
			function combine_string_terms(expr) return {
				switch (expr.eexpr) {
				case TConst(TString(s)): s;
				case TBinop(OpAdd, e1, e2): ^ (combine_string_terms(e1), combine_string_terms(e2));
				case _: "";
				};
			};
			function gen_bin_op(op, expr1, expr2) return {
				var Tuple(expr1, expr2) = switch (op) {
				case Ast.OpAssign | Ast.OpAssignOp(_): (new Tuple(expr1, check_objc_unbox(expr2, expr1.etype)));
				case Ast.OpEq | Ast.OpNotEq: (new Tuple(check_objc_box(expr1, expr2.etype), check_objc_box(expr2, expr1.etype)));
				case _: (new Tuple(expr1, expr2));
				};
				switch (op) {
				case Ast.OpAdd if (&&(is_const_string_term(expr1), is_const_string_term(expr2))): output(str( ^ (combine_string_terms(
								expr1), combine_string_terms(expr2))));
				case Ast.OpAssign: ctx.ctx_assigning = True;
					gen_bin_op_string(expr1, "=", expr2);
				case Ast.OpUShr: output("hx::UShr[");
					gen_expression(True, expr1);
					output(",");
					gen_expression(True, expr2);
					output("]");
				case Ast.OpMod: output("hx::Mod[");
					gen_expression(True, expr1);
					output(",");
					gen_expression(True, expr2);
					output("]");
				case Ast.OpAssignOp(bin_op): output(switch (bin_op) {
				case Ast.OpAdd: "hx::AddEq[";
				case Ast.OpMult: "hx::MultEq[";
				case Ast.OpDiv: "hx::DivEq[";
				case Ast.OpSub: "hx::SubEq[";
				case Ast.OpAnd: "hx::AndEq[";
				case Ast.OpOr: "hx::OrEq[";
				case Ast.OpXor: "hx::XorEq[";
				case Ast.OpShl: "hx::ShlEq[";
				case Ast.OpShr: "hx::ShrEq[";
				case Ast.OpUShr: "hx::UShrEq[";
				case Ast.OpMod: "hx::ModEq[";
				case _: error("Unknown OpAssignOp", expression.epos);
					});
					ctx.ctx_assigning = True;
					gen_expression(True, expr1);
					output(",");
					gen_expression(True, expr2);
					output("]");
				case Ast.OpNotEq: gen_bin_op_string(expr1, "!=", expr2);
				case Ast.OpEq: gen_bin_op_string(expr1, "==", expr2);
				case _: gen_bin_op_string(expr1, Ast.s_binop(op), expr2);
				};
			};
			function gen_array_cast(cast_name, real_type, call) return {
				output( ^ (cast_name, ^ ("< ", ^ (real_type, ^ (" >", call)))));
			};
			function check_array_element_cast(array_type, cast_name, call) return {
				switch (follow(array_type)) {
				case TInst(klass, ::(element, [])): switch (type_string(element)) {
					case _ if (is_struct_access(element)): [];
					case x if (cant_be_null(element)): [];
					case _ if (is_interface_type(element)): [];
					case ::String | Dynamic: [];
					case real_type: gen_array_cast(cast_name, real_type, call);
					};
				case TAbstract(abs, pl) if (<>(abs.a_impl, None)): check_array_element_cast(Abstract.get_underlying_type(abs, pl),
							cast_name, call);
				case _: [];
				};
			};
			function check_array_cast(array_type) return {
				switch (follow(array_type)) {
				case x if (is_interface_type(x)): [];
				case TInst(klass, ::(element, [])): var name = type_string(element);
					if ( && (is_object(name), !(is_interface_type(element)))) {
						gen_array_cast(".StaticCast", "Array<Dynamic>", "[]");
					} else {
						gen_array_cast(".StaticCast", type_string(array_type), "[]");
					};
				case TAbstract(abs, pl) if (<>(abs.a_impl, None)): check_array_cast(Abstract.get_underlying_type(abs, pl));
				case _: [];
				};
			};
			function gen_tfield(field_object, field) return {
				var member = field_name(field);
				var remap_name = keyword_remap(member);
				var already_dynamic = ref(False);
				switch (field_object.eexpr) {
				case TTypeExpr(type_def): switch (get_field_access_meta(field, Meta.Native)) {
					case : var class_name = ^ ("::", join_class_path_remap(t_path(type_def), "::"));
						if ( = (class_name, "::String")) {
							output( ^ ("::String::", remap_name));
						} else {
							output( ^ (class_name, ^ ("_obj::", remap_name)));
						};
					case native: output(native);
					};
				case TLocal({ v_name = __global__ }): output( ^ ("::", member));
				case TConst(TSuper): output(if (ctx.ctx_real_this_ptr) {
					"this";
				} else {
					"__this";
				});
					output( ^ ("->super::", remap_name));
				case TConst(TThis) if (ctx.ctx_real_this_ptr): output( ^ ("this->", remap_name));
				case TConst(TNull): output("null[]");
				case _: gen_expression(True, field_object);
					ctx.ctx_dbgout("/* TField */");
					var settingInternal = && (assigning, = (member, "toString"));
					var isString = = (type_string(field_object.etype), "::String");
					if (is_struct_access(field_object.etype)) {
						output( ^ (".", member));
					} else {
						if ( && (is_internal_member(member), !(settingInternal))) {
							output( ^ (if (isString) {
							".";
						} else {
							"->";
						}, member));
						} else {
							if ( || (settingInternal, is_dynamic_member_lookup_in_cpp(ctx, field_object, field))) {
								if (assigning) {
									output( ^ ("->__FieldRef[", ^ (str(member), "]")));
								} else {
									output( ^ ("->__Field[", ^ (str(member), ", hx::paccDynamic ]")));
								};
								already_dynamic.val = True;
							} else {
								if (isString) {
									output( ^ (".", remap_name));
								} else {
									cast_if_required(ctx, field_object, type_string(field_object.etype));
									var remap_name = if ( = (type_string(field_object.etype), "cpp::ArrayBase")) {
										switch (remap_name) {
										case length: remap_name;
										case _: ^ ("__", remap_name);
										};
									} else {
										remap_name;
									};
									output( ^ ("->", remap_name));
									if ( && (calling, && (is_array(field_object.etype), = (remap_name, "iterator")))) {
										check_array_element_cast(field_object.etype, "Fast", "");
									} else {
										[];
									};
									already_dynamic.val = switch (field) {
									case FInstance(_, _, var) if (is_var_field(var)): True;
									case _: False;
									};
								};
							};
						};
					};
				};
				if ( && (!(already_dynamic.val), && (!(calling), && (!(assigning), is_function_member(expression))))) {
					output("_dyn[]");
				} else {
					[];
				};
			};
			function gen_local_block_call([]) return {
				var func_name = use_anon_function_name(ctx);
				try {
					output( ^ (func_name, ^ ("::Block[", ^ (Hashtbl.find(ctx.ctx_local_return_block_args, func_name), "]"))));
				} catch (e: Not_found) {
					output( ^ ("/* Block function ", ^ (func_name, " not found */")));
				};
			};
			switch (expression.eexpr) {
			case TConst(TNull) if (!(retval)): output("Dynamic[]");
			case TCall(func, arg_list) if (switch (func.eexpr) {
					case TLocal({ v_name = __cpp__ }): True;
						case _: False;
						}):
				switch (arg_list) {
				case ::( {
									 eexpr = TConst(TString(code))
								 }, []): output(format_code(code));
				case ::( {
									 eexpr = TConst(TString(code))
								 } = ecode, tl): Codegen.interpolate_code(ctx.ctx_common, format_code(code), tl, output, gen_expression(True), ecode.epos);
				case _: error("__cpp__'s first argument must be a string", func.epos);
				};
			case TCall(func, arg_list) if (tcall_expand_args): var arg_string = ref("");
				var idx = ref(0);
				List.iter(function arg: var a_name = ^ ("__a", string_of_int(idx.val));
				arg_string.val = ^ (arg_string.val, ^ (if (<>(arg_string.val, "")) {
				",";
			} else {
				"";
			}, a_name));
			idx.val = +(idx.val, 1);
					  output_i( ^ (type_string(arg.etype), ^ (" ", ^ (a_name, " = "))));
					  gen_expression(True, arg);
					  output(";\n"), arg_list);
				output_i(if (retval) {
				"return ";
			} else {
				"";
			});
				ctx.ctx_calling = True;
				gen_expression(True, func);
				output( ^ ("[", ^ (arg_string.val, "];\n")));
			case TCall(func, arg_list) if (is_fromStaticFunction_call(func)):
				switch (arg_list) {
				case ::( {
									 eexpr = TField(_, FStatic(klass, field))
								 }, []): var signature = cpp_function_signature(field.cf_type, "");
					var name = keyword_remap(field.cf_name);
					var void_cast = has_meta_key(field.cf_meta, Meta.Void);
					output( ^ ("::cpp::Function< ", ^ (signature, ">[")));
					if (void_cast) {
						output("hx::AnyCast[");
					} else {
						[];
					};
					output( ^ ("&::", ^ (join_class_path(klass.cl_path, "::"), ^ ("_obj::", name))));
					if (void_cast) {
						output("]");
					} else {
						[];
					};
					output(" ]");
				case _: error("fromStaticFunction must take a static function", expression.epos);
				};
			case TCall({ eexpr = TField(fexpr, field) }, arg_list) if (is_objc_call(field)): output("[ ");
				switch (field) {
				case FStatic(cl, _): output(join_class_path_remap(cl.cl_path, "::"));
				case FInstance(_): gen_expression(True, fexpr);
				case _: assert False;
				};
				var names = ExtString.String.nsplit(field_name(field), ":");
				var Tuple(field_name, arg_names) = switch (names) {
				case ::(name, args): (new Tuple(name, args));
				case _: assert False;
				};
				output( ^ (" ", field_name));
				try {
					switch ((new Tuple(arg_list, arg_names))) {
					case ([], _): [];
					case (::(single_arg, []), _): output(": ");
						gen_expression(True, single_arg);
					case (::(first_arg, args), arg_names): output(": ");
						gen_expression(True, first_arg);
						ctx.ctx_calling = True;
						List.iter2(function arg: function arg_name: output( ^ (" ", ^ (arg_name, ": ")));
								   gen_expression(True, arg), args, arg_names);
					};
				} catch (e: Invalid_argument(_)) {
					error( ^ ("The function called here with name ", ^ (String.concat(":", names),
							  ^ (" does not contain the right amount of arguments' names as required",
								 ^ (" by the objective-c calling / naming convention:", ^ (" expected ", ^ (string_of_int(List.length(arg_list)),
										 ^ (" and found ", string_of_int(List.length(arg_names))))))))), expression.epos);
				};
				output(" ]");
			case TCall(func, ::(arg, [])) if (&&(is_addressOf_call(func), !(is_lvalue(arg)))):
				error("addressOf must take a local or member variable", expression.epos);
			case TCall(func, arg_list): var after_cast = add_objc_cast_if_needed(expression);
				function is_variable(e) return {
					switch (e.eexpr) {
					case TField(_) | TEnumParameter(_): False;
					case TLocal({ v_name = __global__ }): False;
					case TParenthesis(p) | TMeta(_, p): is_variable(p);
					case _: True;
					};
				};
				var expr_type = type_string(expression.etype);
				function is_fixed_override(e) return {
					&& (!(is_scalar(expr_type)), switch (e.eexpr) {
				case TField(obj, FInstance(_, _, field)): var cpp_type = member_type(ctx, obj, field.cf_name);
						&& (!(is_scalar(cpp_type)), var fixed = && (<>(cpp_type, "?"), && (<>(expr_type, "Dynamic"), && (<>(cpp_type, "Dynamic"),
						&& (<>(cpp_type, expr_type), && (<>(expr_type, "Void"), <>(cpp_type, "cpp::ArrayBase"))))));
						if ( && (fixed, > (ctx.ctx_debug_level, 1))) {
						output( ^ ("/* ", ^ (cpp_type, ^ (" != ", ^ (expr_type, " -> cast */")))));
						} else {
							[];
						};
						fixed);
					case TParenthesis(p) | TMeta(_, p): is_fixed_override(p);
					case _: False;
					});
				};
				function check_extern_pointer_cast(e) return {
					switch (remove_parens(e).eexpr) {
					case TField(_, FInstance(class_def, _, _)) | TField(_, FStatic(class_def, _)) if (class_def.cl_extern):
						try {
							var return_type = expression.etype;
							&& (is_pointer(return_type, False), {
								output( ^ (type_string(return_type), "["));
								True;
							});
						} catch (e: Not_found) {
							False;
						};
					case _: False;
					};
				};
				var is_super = switch (func.eexpr) {
				case TConst(TSuper): True;
				case _: False;
				};
				if ( > (ctx.ctx_debug_level, 1)) {
					output( ^ ("/* TCALL ret=", ^ (expr_type, "*/")));
				} else {
					[];
				};
				var cast_result = && (!(is_super), is_fixed_override(func));
				if (cast_result) {
					output( ^ ("hx::TCast< ", ^ (expr_type, " >::cast[")));
				} else {
					[];
				};
				var cast_result = || (cast_result, check_extern_pointer_cast(func));
				var paren_result = if (is_native_with_space(func)) {
					output("[");
					True;
				} else {
					False;
				};
				ctx.ctx_calling = True;
				gen_expression(True, func);
				output("[");
				gen_expression_list(arg_list);
				output("]");
				if (paren_result) {
					output("]");
				} else {
					[];
				};
				if (cast_result) {
					output("]");
				} else {
					[];
				};
				if ( && (is_variable(func), && (!(is_cpp_function_member(func)), && (
													&& (<>(expr_type, "Dynamic"), <>(expr_type, "cpp::ArrayBase")), !(is_super))))) {
					ctx.ctx_output( ^ (".Cast< ", ^ (expr_type, " >[]")));
				} else {
					[];
				};
				function cast_array_output(func) return {
					switch (func.eexpr) {
					case TField(obj, field) if (is_array(obj.etype)):
						switch (field_name(field)) {
						case pop | shift | __unsafe_get | __unsafe_set: check_array_element_cast(obj.etype, ".StaticCast", "[]");
						case map: check_array_cast(expression.etype);
						case _: [];
						};
					case TParenthesis(p) | TMeta(_, p): cast_array_output(p);
					case _: [];
					};
				};
				cast_array_output(func);
				output(after_cast);
			case TBlock(expr_list): if (retval) {
					gen_local_block_call([]);
				} else {
					writerbegin_block;
					dump_src_pos([]);
					var pop_names = push_anon_names(ctx);
					var remaining = ref(List.length(expr_list));
					List.iter(function expression: var want_value = && (return_from_block, = (remaining.val, 1));
							  find_local_functions_and_return_blocks_ctx(want_value, expression);
					if ( > (ctx.ctx_debug_level, 0)) {
					output_i( ^ ("HX_STACK_LINE[", ^ (string_of_int(Lexer.get_error_line(expression.epos)), "]\n")));
					} else {
						[];
					};
					output_i("");
					ctx.ctx_return_from_internal_node = return_from_internal_node;
					if (want_value) {
					output("return ");
					} else {
						[];
					};
					gen_expression(want_value, expression);
					decr(remaining);
					writerterminate_line, expr_list);
					writerend_block;
					pop_names([]);
				};
			case TTypeExpr(type_expr): var klass = ^ ("::", join_class_path_remap(t_path(type_expr), "::"));
				var klass1 = if ( = (klass, "::Array")) {
					"Array<int>";
				} else {
					klass;
				};
				output( ^ ("hx::ClassOf< ", ^ (klass1, " >[]")));
			case TReturn(_) if (retval): unsupported(expression.epos);
			case TReturn(optional_expr): output("");
				switch (optional_expr) {
				case Some(return_expression) if (=(type_string(expression.etype), "Void")): output("return null[");
					gen_expression(True, return_expression);
					output("]");
				case Some(return_expression): output("return ");
					gen_expression(True, return_expression);
				case _: output(if (ctx.ctx_real_void) {
					"return";
				} else {
					"return null[]";
				});
				};
			case TConst(const): switch (const) {
				case TInt(i) if (ctx.ctx_for_extern): output(Printf.sprintf("%ld", i));
				case TInt(i): output(Printf.sprintf("[int]%ld", i));
				case TFloat(float_as_string): output( ^ ("[[Float]", ^ (float_as_string, "]")));
				case TString(s) if (ctx.ctx_for_extern): output( ^ ("\"", ^ (escape_extern(s), "\"")));
				case TString(s): output(str(s));
				case TBool(b): output(if (b) {
					"true";
				} else {
					"false";
				});
				case TNull if (is_objc_type(expression.etype)): output("nil");
				case TNull: output(if (ctx.ctx_for_extern) {
					"null";
				} else {
					"null[]";
				});
				case TThis: output(if (ctx.ctx_real_this_ptr) {
					"hx::ObjectPtr<OBJ_>[this]";
				} else {
					"__this";
				});
				case TSuper if (calling): output(if (ctx.ctx_real_this_ptr) {
					"super::__construct";
				} else {
					^ ("__this->", ^ (ctx.ctx_class_super_name, "::__construct"));
					});
				case TSuper: output( ^ ("hx::ObjectPtr<super>[", ^ (if (ctx.ctx_real_this_ptr) {
					"this";
				} else {
					"__this.mPtr";
				}, "]")));
				};
			case TLocal(v): output(keyword_remap(v.v_name));
			case TArray(array_expr, _) if (is_null(array_expr)): output("Dynamic[]");
			case TArray(array_expr, index): var dynamic =
					|| (is_dynamic_in_cpp(ctx, array_expr), = (type_string(array_expr.etype), "cpp::ArrayBase"));
				if ( && (assigning, !(dynamic))) {
					if (is_array_implementer(array_expr.etype)) {
						output("hx::__ArrayImplRef[");
						gen_expression(True, array_expr);
						output(",");
						gen_expression(True, index);
						output("]");
					} else {
						gen_expression(True, array_expr);
						output("[");
						gen_expression(True, index);
						output("]");
					};
				} else {
					if (assigning) {
						output("hx::IndexRef[[");
						gen_expression(True, array_expr);
						output("].mPtr,");
						gen_expression(True, index);
						output("]");
					} else {
						if (dynamic) {
							gen_expression(True, array_expr);
							output("->__GetItem[");
							gen_expression(True, index);
							output("]");
						} else {
							gen_expression(True, array_expr);
							output("->__get[");
							gen_expression(True, index);
							output("]");
							if (!(is_pointer(array_expr.etype, True))) {
								check_array_element_cast(array_expr.etype, ".StaticCast", "[]");
							} else {
								[];
							};
						};
					};
				};
			case TBinop(op, expr1, expr2): gen_bin_op(op, expr1, expr2);
			case TField(expr, _) | TEnumParameter(expr, _, _) if (is_null(expr)):
				output("hx::Throw[HX_CSTRING[\"Invalid field access on null object\"]]");
			case TEnumParameter(expr, ef, i): var enum = switch (follow(ef.ef_type)) {
				case TEnum(en, _) | TFun(_, TEnum(en, _)): en;
					case _: assert False;
					};
					output( ^ ("[::", ^ (join_class_path_remap(enum.e_path, "::"), "[")));
					gen_expression(True, expr);
					output( ^ ("]]->__Param[", ^ (string_of_int(i), "]")));
				case TField(field_object, field): var after_cast = add_objc_cast_if_needed(expression);
					gen_tfield(field_object, field);
					output(after_cast);
				case TParenthesis(expr) if (!(retval)): gen_expression(retval, expr);
				case TParenthesis(expr): output("[");
					gen_expression(retval, expr);
					output("]");
				case TMeta(_, expr): gen_expression(retval, expr);
				case TObjectDecl(::((fileName, { eexpr = TConst(TString(file)) }), ::((lineNumber, { eexpr = TConst(TInt(line)) }), ::((className, { eexpr = TConst(TString(class_name)) }), ::((methodName, { eexpr = TConst(TString(meth)) }), [])))))
					: output( ^ ("hx::SourceInfo[", ^ (str(file), ^ (",", ^ (Printf.sprintf("%ld", line), ^ (",", ^ (str(class_name), ^ (",",
												   ^ (str(meth), "]")))))))));
			case TObjectDecl(decl_list): gen_local_block_call([]);
			case TArrayDecl(decl_list): var tstr = type_string_suff("_obj", expression.etype, True);
				if ( = (tstr, "Dynamic")) {
					output("Dynamic[ Array_obj<Dynamic>::__new[]");
				} else {
					output( ^ (type_string_suff("_obj", expression.etype, True), "::__new[]"));
				};
				List.iter(function elem: output(".Add[");
						  gen_expression(True, elem);
						  output("]"), decl_list);
				if ( = (tstr, "Dynamic")) {
					output("]");
				} else {
					[];
				};
			case TNew(klass, params, expressions): var is_param_array = switch (klass.cl_path) {
				case ([], Array) if (is_dynamic_array_param(List.hd(params))): True;
				case _: False;
				};
				if (is_param_array) {
					output("Dynamic[ Array_obj<Dynamic>::__new[] ]");
				} else {
					if ( = (klass.cl_path, (new Tuple([], "String")))) {
						output("::String[");
					} else {
						output( ^ (class_string(klass, "_obj", params, True), "::__new["));
					};
					gen_expression_list(expressions);
					output("]");
				};
			case TUnop(Ast.NegBits, Ast.Prefix, expr): output("~[int][");
				gen_expression(True, expr);
				output("]");
			case TUnop(op, Ast.Prefix, expr): ctx.ctx_assigning = switch (op) {
				case Ast.Increment | Ast.Decrement: True;
				case _: False;
				};
				output(Ast.s_unop(op));
				output("[");
				gen_expression(True, expr);
				output("]");
			case TUnop(op, Ast.Postfix, expr): ctx.ctx_assigning = True;
				output("[");
				gen_expression(True, expr);
				output("]");
				output(Ast.s_unop(op));
			case TFunction(func): var func_name = use_anon_function_name(ctx);
				try {
					output( ^ (" Dynamic[new ", ^ (func_name, ^ ("[", ^ (Hashtbl.find(ctx.ctx_local_function_args, func_name), "]]")))));
				} catch (e: Not_found) {
					output( ^ ("function ", ^ (func_name, " not found.")));
				};
			case TVar(tvar, optional_init): var count = ref(1);
				if ( && (retval, == (count.val, 1))) {
					switch (optional_init) {
					case None: output("null[]");
					case Some(expression): gen_expression(True, expression);
					};
				} else {
					var type_name = type_string(tvar.v_type);
					output(if ( = (type_name, "Void")) {
					"Dynamic";
				} else {
					type_name;
				});
					var name = keyword_remap(tvar.v_name);
					output( ^ (" ", name));
					switch (optional_init) {
					case None: [];
					case Some(expression): output(" = ");
						gen_expression(True, expression);
					};
					count.val = -(count.val, 1);
					if ( > (ctx.ctx_debug_level, 0)) {
						output( ^ (";\t\tHX_STACK_VAR[", ^ (name, ^ (",\"", ^ (tvar.v_name, "\"]")))));
					} else {
						[];
					};
					if ( > (count.val, 0)) {
						output(";\n");
						output_i("");
					} else {
						[];
					};
				};
			case TFor(tvar, init, loop): output( ^ ("for[::cpp::FastIterator_obj< ", ^ (type_string(tvar.v_type),
														^ (" > *__it = ::cpp::CreateFastIterator< ", ^ (type_string(tvar.v_type), " >[")))));
				gen_expression(True, init);
				output("];  __it->hasNext[]; ]");
				ctx.ctx_writerbegin_block;
				output_i( ^ (type_string(tvar.v_type), ^ (" ", ^ (keyword_remap(tvar.v_name), " = __it->next[];\n"))));
				output_i("");
				gen_expression(False, loop);
				output(";\n");
				ctx.ctx_writerend_block;
			case TIf(condition, if_expr, optional_else_expr): switch (optional_else_expr) {
				case Some(else_expr): if (retval) {
						output("[  [");
						gen_expression(True, condition);
						output("] ? ");
						var type_str = switch (type_string(expression.etype)) {
						case Void: "Dynamic";
						case other: other;
						};
						output( ^ (type_str, "["));
						gen_expression(True, if_expr);
						output("] : ");
						output( ^ (type_str, "["));
						gen_expression(True, else_expr);
						output("] ]");
					} else {
						output("if [");
						gen_expression(True, condition);
						output("]");
						gen_block_expression(if_expr);
						output_i("else");
						gen_block_expression(else_expr);
					};
				case _: output("if [");
					gen_expression(True, condition);
					output("]");
					gen_block_expression(if_expr);
				};
			case TWhile(condition, repeat, Ast.NormalWhile): output("while[");
				gen_expression(True, condition);
				output("]");
				gen_block_expression(repeat);
			case TWhile(condition, repeat, Ast.DoWhile): output("do");
				gen_block_expression(repeat);
				output("while[");
				gen_expression(True, condition);
				output("]");
			case TTry(_, _) | TSwitch(_, _, _) if (&&(retval, !(return_from_internal_node))): gen_local_block_call([]);
			case TSwitch(condition, cases, optional_default): var switch_on_int_constants =
					&& (only_int_cases(cases), !(contains_break(expression)));
				if (switch_on_int_constants) {
					output("switch[ [int]");
					gen_expression(True, condition);
					output("]");
					ctx.ctx_writerbegin_block;
					List.iter(function (cases_list, expression): output_i("");
							  List.iter(function value: output("case ");
										gen_expression(True, value);
										output(": "), cases_list);
							  ctx.ctx_return_from_block = return_from_internal_node;
							  gen_block_expression(expression);
							  output_i(";break;\n"), cases);
					switch (optional_default) {
					case None: [];
					case Some(default): output_i("default: ");
						ctx.ctx_return_from_block = return_from_internal_node;
						gen_block_expression(default);
					};
					ctx.ctx_writerend_block;
				} else {
					var tmp_name = get_switch_var(ctx);
					output( ^ (type_string(condition.etype), ^ (" ", ^ (tmp_name, " = "))));
					gen_expression(True, condition);
					output(";\n");
					var else_str = ref("");
					if ( > (List.length(cases), 0)) {
						List.iter(function (cases, expression): output_i( ^ (else_str.val, "if [ "));
								  else_str.val = "else ";
								  var or_str = ref("");
								  List.iter(function value: output( ^ (or_str.val, ^ (", [ ", ^ (tmp_name, "=="))));
											gen_expression(True, value);
											output("]");
											or_str.val = " || ", cases);
								  output("]");
								  ctx.ctx_return_from_block = return_from_internal_node;
								  gen_block_expression(expression), cases);
					} else {
						[];
					};
					switch (optional_default) {
					case None: [];
					case Some(default): output_i( ^ (else_str.val, " "));
						ctx.ctx_return_from_block = return_from_internal_node;
						gen_block_expression(default);
						output(";\n");
					};
				};
			case TTry(expression, catch_list): output("try\n");
				output_i("{\n");
				var counter = ref(0);
				List.iter(function (v, e): var type_name = type_string(v.v_type);
						  output_i( ^ ("HX_STACK_CATCHABLE[", ^ (type_name, ^ (", ", ^ (string_of_int(counter.val), "];\n")))));
						  counter.val = +(counter.val, 1), catch_list);
				output_i("");
				ctx.ctx_return_from_block = return_from_internal_node;
				gen_block_expression(expression);
				output_i("}\n");
				if ( > (List.length(catch_list), 0)) {
					output_i("catch[Dynamic __e]");
					ctx.ctx_writerbegin_block;
					var seen_dynamic = ref(False);
					var else_str = ref("");
					List.iter(function (v, expression): var type_name = type_string(v.v_type);
					if ( = (type_name, "Dynamic")) {
					seen_dynamic.val = True;
					output_i(else_str.val);
					} else {
						output_i( ^ (else_str.val, ^ ("if [__e.IsClass< ", ^ (type_name, " >[] ]"))));
					};
					ctx.ctx_writerbegin_block;
					output_i("HX_STACK_BEGIN_CATCH\n");
					output_i( ^ (type_name, ^ (" ", ^ (v.v_name, " = __e;"))));
					ctx.ctx_return_from_block = return_from_internal_node;
												gen_block_expression(mk_block(expression));
												ctx.ctx_writerend_block;
												else_str.val = "else ", catch_list);
					if (!(seen_dynamic.val)) {
						output_i("else {\n");
						output_i("    HX_STACK_DO_THROW[__e];\n");
						output_i("}\n");
					} else {
						[];
					};
					ctx.ctx_writerend_block;
				} else {
					[];
				};
			case TBreak: output("break");
			case TContinue: output("continue");
			case TThrow(expression): output("HX_STACK_DO_THROW[");
				gen_expression(True, expression);
				output("]");
			case TCast(cast, None) if (&&(is_objc_type(expression.etype), !(is_objc_type(cast.etype)))): var ret_type = type_string(
							expression.etype);
				output( ^ ("[ [", ^ (ret_type, "] [id] [")));
				gen_expression(True, cast);
				output("] ]");
			case TCast(cast, None) if (||(!(retval), =(type_string(expression.etype), "Void"))): gen_expression(retval, cast);
			case TCast(cast, None): var ret_type = type_string(expression.etype);
				var from_type = if (is_dynamic_in_cpp(ctx, cast)) {
					"Dynamic";
				} else {
					type_string(cast.etype);
				};
				if ( = (from_type, ret_type)) {
					gen_expression(True, cast);
				} else {
					output( ^ ("[[", ^ (ret_type, "][")));
					gen_expression(True, cast);
					output("]]");
				};
			case TCast(e1, Some(t)): var class_name = join_class_path_remap(t_path(t), "::");
				if ( = (class_name, "Array")) {
					output("hx::TCastToArray[");
				} else {
					output( ^ ("hx::TCast< ::", ^ (class_name, " >::cast[")));
				};
				gen_expression(True, e1);
				output("]");
			};
		};
		function gen_block_expression(expression) return {
			gen_expression(False, mk_block(expression));
		};
		if (<>(set_var, "")) {
			find_local_functions_and_return_blocks_ctx(True, expression_tree);
			output(set_var);
		} else {
			[];
		};
		gen_expression(retval, expression_tree);
		output(tail_code);
	};

	public static function is_dynamic_haxe_method(f) return {
		switch ((new Tuple(f.cf_expr, f.cf_kind))) {
		case (Some({ eexpr = TFunction(_) }), Var(_) | Method(MethDynamic)): True;
		case _: False;
		};
	};

	public static function is_data_member(field) return {
		switch (field.cf_expr) {
		case Some({ eexpr = TFunction(function_def) }): is_dynamic_haxe_method(field);
		case _: True;
		};
	};

	public static function is_override(class_def, field) return {
		List.exists(function f: = (f.cf_name, field), class_def.cl_overrides);
	};

	public static function all_virtual_functions(clazz) return {
		@(List.fold_left(function result: function elem:
		switch ((new Tuple(follow(elem.cf_type), elem.cf_kind))) {
	case (_, Method(MethDynamic)): result;
		case (TFun(args, return_type), Method(_)) if (!(is_override(clazz, elem.cf_name))): ::((new Tuple(elem, args, return_type)), result);
		case (_, _): result;
		}, [], clazz.cl_ordered_fields), switch (clazz.cl_super) {
	case Some(def): all_virtual_functions(fst(def));
		case _: [];
		});
	};

	public static function reflective(class_def, field) return {
		!( || (Meta.has(Meta.NativeGen, class_def.cl_meta), || (Meta.has(Meta.Unreflective, class_def.cl_meta), || (Meta.has(Meta.Unreflective, field.cf_meta), switch (field.cf_type) {
	case TInst(klass, _): Meta.has(Meta.Unreflective, klass.cl_meta);
		case _: False;
		}))));
	};

	public static function field_arg_count(field) return {
		switch ((new Tuple(follow(field.cf_type), field.cf_kind))) {
		case (_, Method(MethDynamic)): -1;
		case (TFun(args, return_type), Method(_)): List.length(args);
		case (_, _): -1;
		};
	};

	public static function gen_field(ctx, class_def, class_name, ptr_name, dot_name, is_static, is_interface, field) return {
		var output = ctx.ctx_output;
		ctx.ctx_real_this_ptr = !(is_static);
		var remap_name = keyword_remap(field.cf_name);
		var decl = get_meta_string(field.cf_meta, Meta.Decl);
		var has_decl = <>(decl, "");
		var nativeGen = has_meta_key(class_def.cl_meta, Meta.NativeGen);
		if (is_interface) {
			[];
		} else {
			switch (field.cf_expr) {
			case Some({ eexpr = TFunction(function_def) }): var return_type = type_string(function_def.tf_type);
				var nargs = string_of_int(List.length(function_def.tf_args));
				var is_void = = (type_string(function_def.tf_type), "Void");
				var ret = if (is_void) {
					"[void]";
				} else {
					"return ";
				};
				var output_i = ctx.ctx_writerwrite_i;
				var orig_debug = ctx.ctx_debug_level;
				var dump_src = if ( || (Meta.has(Meta.NoStack, field.cf_meta), || (Meta.has(Meta.NoDebug, field.cf_meta),
				|| ( < (orig_debug, 1), nativeGen)))) {
					ctx.ctx_debug_level = 0;
					function []: [];
				} else {
					function []: hx_stack_push(ctx, output_i, dot_name, field.cf_name, function_def.tf_expr.epos);
					if (!(is_static)) {
						output_i("HX_STACK_THIS[this]\n");
					} else {
						[];
					};
					List.iter(function (v, _): output_i( ^ ("HX_STACK_ARG[", ^ (keyword_remap(v.v_name), ^ (",\"", ^ (v.v_name, "\"]\n"))))),
							  function_def.tf_args);
				};
				if (!(is_dynamic_haxe_method(field))) {
					var real_void = && (is_void, has_meta_key(field.cf_meta, Meta.Void));
					var fake_void = && (is_void, !(real_void));
					output(if (real_void) {
					"void";
				} else {
					return_type;
				});
					output( ^ (" ", ^ (class_name, ^ ("::", ^ (remap_name, "[")))));
					output(gen_arg_list(function_def.tf_args, "__o_"));
					output("]\n");
					ctx.ctx_real_this_ptr = True;
					ctx.ctx_real_void = real_void;
					ctx.ctx_dynamic_this_ptr = False;
					var code = get_code(field.cf_meta, Meta.FunctionCode);
					var tail_code = get_code(field.cf_meta, Meta.FunctionTailCode);
					if (has_default_values(function_def.tf_args)) {
						ctx.ctx_writerbegin_block;
						generate_default_values(ctx, function_def.tf_args, "__o_");
						dump_src([]);
						output(code);
						gen_expression_tree(ctx, False, function_def.tf_expr, "", tail_code);
						if (fake_void) {
							output("\treturn null[];\n");
						} else {
							[];
						};
						ctx.ctx_writerend_block;
					} else {
						var add_block = || (is_void, || (<>(code, ""), <>(tail_code, "")));
						if (add_block) {
							ctx.ctx_writerbegin_block;
						} else {
							[];
						};
						ctx.ctx_dump_src_pos = dump_src;
						output(code);
						gen_expression_tree(ctx, False, mk_block(function_def.tf_expr), "", tail_code);
						if (add_block) {
							if (fake_void) {
								output("\treturn null[];\n");
							} else {
								[];
							};
							ctx.ctx_writerend_block;
						} else {
							[];
						};
					};
					output("\n\n");
					var nonVirtual = has_meta_key(field.cf_meta, Meta.NonVirtual);
					var doDynamic = && ( || (nonVirtual, !(is_override(class_def, field.cf_name))), reflective(class_def, field));
					if (doDynamic) {
						if (is_static) {
							output("STATIC_");
						} else {
							[];
						};
						output( ^ ("HX_DEFINE_DYNAMIC_FUNC", ^ (nargs, ^ ("[", ^ (class_name, ^ (",", ^ (remap_name, ^ (",", ^ (ret,
																"]\n\n")))))))));
					} else {
						[];
					};
				} else {
					ctx.ctx_real_this_ptr = False;
					ctx.ctx_dynamic_this_ptr = False;
					var func_name = ^ ("__default_", remap_name);
					output( ^ ("HX_BEGIN_DEFAULT_FUNC[", ^ (func_name, ^ (",", ^ (class_name, "]\n")))));
					output(return_type);
					output( ^ (" run[", ^ (gen_arg_list(function_def.tf_args, "__o_"), "]")));
					ctx.ctx_dump_src_pos = dump_src;
					if (is_void) {
						ctx.ctx_writerbegin_block;
						generate_default_values(ctx, function_def.tf_args, "__o_");
						gen_expression_tree(ctx, False, function_def.tf_expr, "", "");
						output("return null[];\n");
						ctx.ctx_writerend_block;
					} else {
						if (has_default_values(function_def.tf_args)) {
							ctx.ctx_writerbegin_block;
							generate_default_values(ctx, function_def.tf_args, "__o_");
							gen_expression_tree(ctx, False, function_def.tf_expr, "", "");
							ctx.ctx_writerend_block;
						} else {
							gen_expression_tree(ctx, False, mk_block(function_def.tf_expr), "", "");
						};
					};
					output( ^ ("HX_END_LOCAL_FUNC", ^ (nargs, ^ ("[", ^ (ret, "]\n")))));
					output("HX_END_DEFAULT_FUNC\n\n");
					if (is_static) {
						output( ^ ("Dynamic ", ^ (class_name, ^ ("::", ^ (remap_name, ";\n\n")))));
					} else {
						[];
					};
				};
				ctx.ctx_debug_level = orig_debug;
			case _ if (has_decl):
				if (is_static) {
					output( ^ (class_name, ^ ("::", ^ (remap_name, "_decl "))));
					output( ^ (" ", ^ (class_name, ^ ("::", ^ (remap_name, ";\n\n")))));
				} else {
					[];
				};
			case _: if ( && (is_static, !(is_extern_field(field)))) {
					gen_type(ctx, field.cf_type);
					output( ^ (" ", ^ (class_name, ^ ("::", ^ (remap_name, ";\n\n")))));
				} else {
					[];
				};
			};
		};
	};

	public static function gen_field_init(ctx, field) return {
		var output = ctx.ctx_output;
		var remap_name = keyword_remap(field.cf_name);
		switch (field.cf_expr) {
		case Some({ eexpr = TFunction(function_def) }):
			if (is_dynamic_haxe_method(field)) {
				var func_name = ^ ("__default_", remap_name);
				output( ^ ("\t", ^ (remap_name, ^ (" = new ", ^ (func_name, ";\n\n")))));
			} else {
				[];
			};
		case _: switch (field.cf_expr) {
			case Some(expr): var var_name = switch (remap_name) {
				case __meta__: "\t__mClass->__meta__=";
				case __rtti: "\t__mClass->__rtti__=";
				case _: ^ ("\t", ^ (remap_name, "= "));
				};
				gen_expression_tree(ctx, True, expr, var_name, ";\n");
			case _: [];
			};
		};
	};

	public static function has_field_init(field) return {
		switch (field.cf_expr) {
		case Some({ eexpr = TFunction(function_def) }): is_dynamic_haxe_method(field);
		case Some(_): True;
		case _: False;
		};
	};

	public static function gen_member_def(ctx, class_def, is_static, is_interface, field) return {
		var output = ctx.ctx_output;
		var remap_name = keyword_remap(field.cf_name);
		var nativeGen = has_meta_key(class_def.cl_meta, Meta.NativeGen);
		if (is_interface) {
			switch ((new Tuple(follow(field.cf_type), field.cf_kind))) {
			case (_, Method(MethDynamic)): [];
			case (TFun(args, return_type), Method(_)): output( ^ (if (!(is_static)) {
				"		virtual ";
			} else {
				"		";
			}, type_string(return_type)));
				output( ^ (" ", ^ (remap_name, "[ ")));
				output(gen_tfun_interface_arg_list(args));
				output(if (!(is_static)) {
				"]=0;\n";
			} else {
				"];\n";
			});
				if (reflective(class_def, field)) {
					output( ^ ("virtual Dynamic ", ^ (remap_name, "_dyn[]=0;\n")));
				} else {
					[];
				};
			case _: [];
			};
		} else {
			var decl = get_meta_string(field.cf_meta, Meta.Decl);
			var has_decl = <>(decl, "");
			if (has_decl) {
				output( ^ ("      typedef ", ^ (decl, ";\n")));
			} else {
				[];
			};
			output(if (is_static) {
			"\t\tstatic ";
		} else {
			"\t\t";
		});
			switch (field.cf_expr) {
			case Some({ eexpr = TFunction(function_def) }): var nonVirtual = has_meta_key(field.cf_meta, Meta.NonVirtual);
				var doDynamic = && ( || (nonVirtual, !(is_override(class_def, field.cf_name))), reflective(class_def, field));
				if (is_dynamic_haxe_method(field)) {
					if (doDynamic) {
						output( ^ ("Dynamic ", ^ (remap_name, ";\n")));
						output(if (is_static) {
						"\t\tstatic ";
					} else {
						"\t\t";
					});
						output( ^ ("inline Dynamic &", ^ (remap_name, ^ ("_dyn[] ", ^ ("{return ", ^ (remap_name, "; }\n"))))));
					} else {
						[];
					};
				} else {
					var return_type = type_string(function_def.tf_type);
					if ( && (!(is_static), !(nonVirtual))) {
						output("virtual ");
					} else {
						[];
					};
					output(if ( && ( = (return_type, "Void"), has_meta_key(field.cf_meta, Meta.Void))) {
					"void";
				} else {
					return_type;
				});
					output( ^ (" ", ^ (remap_name, "[")));
					output(gen_arg_list(function_def.tf_args, ""));
					output("];\n");
					if (doDynamic) {
						output(if (is_static) {
						"\t\tstatic ";
					} else {
						"\t\t";
					});
						output( ^ ("Dynamic ", ^ (remap_name, "_dyn[];\n")));
					} else {
						[];
					};
				};
				output("\n");
			case _ if (has_decl): output( ^ (remap_name, ^ ("_decl ", ^ (remap_name, ";\n"))));
			case _: gen_type(ctx, field.cf_type);
				output( ^ (" ", ^ (remap_name, ";\n")));
				switch (follow(field.cf_type)) {
				case _ if (nativeGen): [];
				case TFun(_, _): output(if (is_static) {
					"\t\tstatic ";
				} else {
					"\t\t";
				});
					gen_type(ctx, field.cf_type);
					output( ^ (" &", ^ (remap_name, ^ ("_dyn[] { return ", ^ (remap_name, ";}\n")))));
				case _: switch (field.cf_kind) {
					case Var({ v_read = AccCall }) if (&&(!(is_static), is_dynamic_accessor(^("get_", field.cf_name), "get", field, class_def)))
								: output( ^ ("\t\tDynamic get_", ^ (field.cf_name, ";\n")));
					case _: [];
					};
					switch (field.cf_kind) {
					case Var({ v_write = AccCall }) if (&&(!(is_static), is_dynamic_accessor(^("set_", field.cf_name), "set", field, class_def)))
								: output( ^ ("\t\tDynamic set_", ^ (field.cf_name, ";\n")));
					case _: [];
					};
				};
			};
		};
	};

	public static function path_of_string(path) return {
		(new Tuple(::("@verbatim", []), path));
	};

	public static function find_referenced_types(ctx, obj, super_deps, constructor_deps, header_only, for_depends,
			include_super_args) return {
		var types = ref(PMap.empty);
		function add_type(in_path) return {
			if (!(PMap.mem(in_path, types.val))) {
				types.val = PMap.add(in_path, [], types.val);
				try {
					List.iter(add_type, Hashtbl.find(super_deps, in_path));
				} catch (e: Not_found) {
					[];
				};
			} else {
				[];
			};
		};
		function add_extern_class(klass) return {
			var include_file = get_meta_string_path(klass.cl_meta, if (for_depends) {
			Meta.Depend;
		} else {
			Meta.Include;
		});
			if (<>(include_file, "")) {
				add_type(path_of_string(include_file));
			} else {
				if ( && (!(for_depends), has_meta_key(klass.cl_meta, Meta.Include))) {
					add_type(klass.cl_path);
				} else {
					[];
				};
			};
		};
		function add_native_gen_class(klass) return {
			var include_file = get_meta_string_path(klass.cl_meta, if (for_depends) {
			Meta.Depend;
		} else {
			Meta.Include;
		});
			if (<>(include_file, "")) {
				add_type(path_of_string(include_file));
			} else {
				if (for_depends) {
					add_type(klass.cl_path);
				} else {
					add_type(path_of_string( ^ (join_class_path(klass.cl_path, "/"), ".h")));
				};
			};
		};
		var visited = ref([]);
		function visit_type(in_type) return {
			if (!(List.exists(function t2: Type.fast_eq(in_type, t2), visited.val))) {
				visited.val = ::(in_type, visited.val);
				switch (follow(in_type)) {
				case TMono(r): switch (r.val) {
					case None: [];
					case Some(t): visit_type(t);
					};
				case TEnum(enum, params): add_type(enum.e_path);
				case TInst(klass, params): switch (klass.cl_path) {
					case ([], Array) | ([], Class) | (::(cpp, []), FastIterator) | (::(cpp, []), Pointer) | (::(cpp, []), ConstPointer) | (::(cpp, []), Function) | (::(cpp, []), RawPointer) | (::(cpp, []), RawConstPointer)
							: List.iter(visit_type, params);
					case _ if (is_native_gen_class(klass)): add_native_gen_class(klass);
					case _ if (is_extern_class(klass)): add_extern_class(klass);
					case _: switch (klass.cl_kind) {
						case KTypeParameter(_): [];
						case _: add_type(klass.cl_path);
						};
					};
				case TFun(args, haxe_type): visit_type(haxe_type);
					List.iter(function (_, _, t): visit_type(t), args);
				case _: [];
				};
				visited.val = List.tl(visited.val);
			} else {
				[];
			};
		};
		function visit_params(expression) return {
			function visit_expression(expression) return {
				switch (expression.eexpr) {
				case TTypeExpr(type_def): switch (type_def) {
					case TClassDecl(class_def) if (is_native_gen_class(class_def)): add_native_gen_class(class_def);
					case TClassDecl(class_def) if (is_extern_class(class_def)): add_extern_class(class_def);
					case _: add_type(t_path(type_def));
					};
				case TTry(e, catches): List.iter(function (v, _): visit_type(v.v_type), catches);
				case TNew(klass, params, _): visit_type(TInst(klass, params));
					try {
						var construct_type = Hashtbl.find(constructor_deps, klass.cl_path);
						visit_type(construct_type.cf_type);
					} catch (e: Not_found) {
						[];
					};
				case TVar(v, _): visit_type(v.v_type);
				case TEnumParameter(_, ef, _): visit_type(follow(ef.ef_type));
				case TFunction(func_def): List.iter(function (v, _): visit_type(v.v_type), func_def.tf_args);
				case TConst(TSuper): switch (follow(expression.etype)) {
					case TInst(klass, params): try {
							var construct_type = Hashtbl.find(constructor_deps, klass.cl_path);
							visit_type(construct_type.cf_type);
						} catch (e: Not_found) {
							[];
						};
					case _: print_endline( ^ ("TSuper : Odd etype ?", type_string(expression.etype)));
					};
				case _: [];
				};
				Type.iter(visit_expression, expression);
				visit_type(follow(expression.etype));
			};
			visit_expression(expression);
		};
		function visit_field(field) return {
			visit_type(field.cf_type);
			if (!(header_only)) {
				switch (field.cf_expr) {
				case Some(expression): visit_params(expression);
				case _: [];
				};
			} else {
				[];
			};
		};
		function visit_class(class_def) return {
			var fields = List.append(class_def.cl_ordered_fields, class_def.cl_ordered_statics);
			var fields_and_constructor = List.append(fields, switch (class_def.cl_constructor) {
		case Some(expr): ::(expr, []);
			case _: [];
			});
			List.iter(visit_field, fields_and_constructor);
			if (include_super_args) {
				List.iter(visit_field, List.map(function (a, _, _): a, all_virtual_functions(class_def)));
			} else {
				[];
			};
			if (is_native_gen_class(class_def)) {
				add_native_gen_class(class_def);
			} else {
				add_type(class_def.cl_path);
			};
		};
		function visit_enum(enum_def) return {
			add_type(enum_def.e_path);
			PMap.iter(function _: function constructor:
			switch (constructor.ef_type) {
		case TFun(args, _): List.iter(function (_, _, t): visit_type(t), args);
			case _: [];
			}, enum_def.e_constrs);
			if (!(header_only)) {
				var meta = Codegen.build_metadata(ctx, TEnumDecl(enum_def));
				switch (meta) {
				case Some(expr): visit_params(expr);
				case _: [];
				};
			} else {
				[];
			};
		};
		function inc_cmp(i1, i2) return {
			String.compare(join_class_path(i1, "."), join_class_path(i2, "."));
		};
		switch (obj) {
		case TClassDecl(class_def): visit_class(class_def);
			switch (class_def.cl_init) {
			case Some(expression): visit_params(expression);
			case _: [];
			};
		case TEnumDecl(enum_def): visit_enum(enum_def);
		case TTypeDecl(_) | TAbstractDecl(_): [];
		};
		List.sort(inc_cmp, List.filter(function path: include_class_header(path), pmap_keys(types.val)));
	};

	public static function generate_main_header(output_main) return {
		output_main("#include <hxcpp.h>\n\n");
		output_main("#include <stdio.h>\n\n");
		output_main("extern \"C\" void __hxcpp_main[];\n\n");
		output_main("extern \"C\" void __hxcpp_lib_main[];\n\n");
	};

	public static function generate_main_footer1(output_main) return {
		output_main("void __hxcpp_main[] {\n");
	};

	public static function generate_main_footer2(output_main) return {
		output_main("	}\n\n");
		output_main("void __hxcpp_lib_main[] {\n");
		output_main("	HX_TOP_OF_STACK\n");
		output_main("	hx::Boot[];\n");
		output_main("	__boot_all[];\n");
		output_main("	__hxcpp_main[];\n");
		output_main("	}\n");
	};

	public static function generate_main(common_ctx, member_types, super_deps, class_def, file_info) return {
		var main_expression = switch (class_def.cl_ordered_statics) {
		case ::( {
				cf_expr = Some(expression)
			}, []): expression;
		case _: assert False;
		};
		ignore(find_referenced_types(common_ctx, TClassDecl(class_def), super_deps, Hashtbl.create(0), False, False, False));
		var depend_referenced = find_referenced_types(common_ctx, TClassDecl(class_def), super_deps, Hashtbl.create(0), False, True, False);
		function generate_startup(filename, is_main) return {
			var cpp_file = new_cpp_file(common_ctx, common_ctx.file, (new Tuple([], filename)));
			var output_main = cpp_filewrite;
			generate_main_header(output_main);
			List.iter(add_include(cpp_file), depend_referenced);
			output_main("\n\n");
			if (is_main) {
				output_main("\n#include <hx/HxcppMain.h>\n\n");
			} else {
				[];
			};
			generate_main_footer1(output_main);
			gen_expression_tree(new_context(common_ctx, cpp_file, 1, file_info), False, main_expression, "", ";\n");
			generate_main_footer2(output_main);
			cpp_fileclose;
		};
		generate_startup("__main__", True);
		generate_startup("__lib__", False);
	};

	public static function generate_dummy_main(common_ctx) return {
		function generate_startup(filename, is_main) return {
			var main_file = new_cpp_file(common_ctx, common_ctx.file, (new Tuple([], filename)));
			var output_main = main_filewrite;
			generate_main_header(output_main);
			if (is_main) {
				output_main("\n#include <hx/HxcppMain.h>\n\n");
			} else {
				[];
			};
			generate_main_footer1(output_main);
			generate_main_footer2(output_main);
			main_fileclose;
		};
		generate_startup("__main__", True);
		generate_startup("__lib__", False);
	};

	public static function generate_boot(common_ctx, boot_enums, boot_classes, nonboot_classes, init_classes) return {
		var base_dir = common_ctx.file;
		var boot_file = new_cpp_file(common_ctx, base_dir, (new Tuple([], "__boot__")));
		var output_boot = boot_filewrite;
		output_boot("#include <hxcpp.h>\n\n");
		List.iter(function class_path: boot_fileadd_include(class_path), @(boot_enums, @(boot_classes, nonboot_classes)));
		output_boot("\nvoid __files__boot[];\n");
		output_boot("\nvoid __boot_all[]\n{\n");
		output_boot("__files__boot[];\n");
		output_boot("hx::RegisterResources[ hx::GetResources[] ];\n");
		List.iter(function class_path: output_boot( ^ ("::", ^ (join_class_path_remap(class_path, "::"), "_obj::__register[];\n"))), @(boot_enums, @(boot_classes, nonboot_classes)));
		var dump_boot = List.iter(function class_path: output_boot( ^ ("::", ^ (join_class_path_remap(class_path, "::"), "_obj::__boot[];\n"))));
		dump_boot(boot_enums);
		List.iter(function class_path: output_boot( ^ ("::", ^ (join_class_path_remap(class_path, "::"), "_obj::__init__[];\n"))), List.rev(init_classes));
		dump_boot(List.filter(function path: is_cpp_class(path), List.rev(boot_classes)));
		dump_boot(List.filter(function path: !(is_cpp_class(path)), List.rev(boot_classes)));
		output_boot("}\n\n");
		boot_fileclose;
	};

	public static function generate_files(common_ctx, file_info) return {
		var base_dir = common_ctx.file;
		var files_file = new_cpp_file(common_ctx, base_dir, (new Tuple([], "__files__")));
		var output_files = files_filewrite;
		var types = common_ctx.types;
		output_files("#include <hxcpp.h>\n\n");
		output_files("namespace hx {\n");
		output_files("const char *__hxcpp_all_files[] = {\n");
		output_files("#ifdef HXCPP_DEBUGGER\n");
		List.iter(function file: output_files( ^ (const_char_star(file), ",\n")), List.sort(String.compare, pmap_keys(file_info.val)));
		output_files("#endif\n");
		output_files(" 0 };\n");
		output_files("\n");
		output_files("const char *__hxcpp_all_files_fullpath[] = {\n");
		output_files("#ifdef HXCPP_DEBUGGER\n");
		List.iter(function file: output_files( ^ (const_char_star(Common.get_full_path(try {
			Common.find_file(common_ctx, file);
		} catch (e: Not_found) {
			file;
		})), ",\n")), List.sort(String.compare, pmap_keys(file_info.val)));
		output_files("#endif\n");
		output_files(" 0 };\n");
		output_files("\n");
		output_files("const char *__hxcpp_all_classes[] = {\n");
		output_files("#ifdef HXCPP_DEBUGGER\n");
		List.iter(function object_def:
		switch (object_def) {
	case TClassDecl(class_def) if (is_extern_class(class_def)): [];
		case TClassDecl(class_def) if (class_def.cl_interface): [];
		case TClassDecl(class_def): output_files( ^ (const_char_star(join_class_path(class_def.cl_path, ".")), ",\n"));
		case _: [];
		}, types);
		output_files("#endif\n");
		output_files(" 0 };\n");
		output_files("} // namespace hx\n");
		output_files("void __files__boot[] { __hxcpp_set_debugger_info[hx::__hxcpp_all_classes, hx::__hxcpp_all_files_fullpath]; }\n");
		files_fileclose;
	};

	public static function begin_header_file(output_h, def_string) return {
		output_h( ^ ("#ifndef INCLUDED_", ^ (def_string, "\n")));
		output_h( ^ ("#define INCLUDED_", ^ (def_string, "\n\n")));
		output_h("#ifndef HXCPP_H\n");
		output_h("#include <hxcpp.h>\n");
		output_h("#endif\n\n");
	};

	public static function end_header_file(output_h, def_string) return {
		output_h( ^ ("\n#endif /* INCLUDED_", ^ (def_string, " */ \n")));
	};

	public static function new_placed_cpp_file(common_ctx, class_path) return {
		var base_dir = common_ctx.file;
		if (Common.defined(common_ctx, Define.Vcproj)) {
			make_class_directories(base_dir, ::("src", []));
			cached_source_writer(common_ctx, ^ (base_dir, ^ ("/src/", ^ (String.concat("-", fst(class_path)), ^ ("-",
			^ (snd(class_path), source_file_extension(common_ctx)))))));
		} else {
			new_cpp_file(common_ctx, common_ctx.file, class_path);
		};
	};

	public static function generate_enum_files(common_ctx, enum_def, super_deps, meta, file_info) return {
		var class_path = enum_def.e_path;
		var just_class_name = snd(class_path);
		var class_name = ^ (just_class_name, "_obj");
		var remap_class_name = ^ ("::", join_class_path_remap(class_path, "::"));
		var cpp_file = new_placed_cpp_file(common_ctx, class_path);
		var output_cpp = cpp_filewrite;
		var debug = if ( || (has_meta_key(enum_def.e_meta, Meta.NoDebug), Common.defined(common_ctx, Define.NoDebug))) {
			0;
		} else {
			1;
		};
		var ctx = new_context(common_ctx, cpp_file, debug, file_info);
		if ( > (debug, 1)) {
			print_endline( ^ ("Found enum definition:", join_class_path(class_path, "::")));
		} else {
			[];
		};
		output_cpp("#include <hxcpp.h>\n\n");
		var referenced = find_referenced_types(common_ctx, TEnumDecl(enum_def), super_deps, Hashtbl.create(0), False, False, False);
		List.iter(add_include(cpp_file), referenced);
		gen_open_namespace(output_cpp, class_path);
		output_cpp("\n");
		PMap.iter(function _: function constructor: var name = keyword_remap(constructor.ef_name);
		switch (constructor.ef_type) {
	case TFun(args, _): output_cpp( ^ (remap_class_name, ^ (" ", ^ (class_name, ^ ("::", ^ (name, ^ ("[",
											   ^ (gen_tfun_arg_list(args), "]\n"))))))));
			output_cpp( ^ ("{\n\treturn hx::CreateEnum< ", ^ (class_name, ^ (" >[", ^ (str(name), ^ (",",
						   ^ (string_of_int(constructor.ef_index), ^ (",hx::DynamicArray[0,", ^ (string_of_int(List.length(args)), "]")))))))));
			List.iter(function (arg, _, _): output_cpp( ^ (".Add[", ^ (keyword_remap(arg), "]"))), args);
			output_cpp("];\n}\n\n");
		case _: output_cpp( ^ (remap_class_name, ^ (" ", ^ (class_name, ^ ("::", ^ (name, ";\n\n"))))));
		}, enum_def.e_constrs);
		output_cpp( ^ ("HX_DEFINE_CREATE_ENUM[", ^ (class_name, "]\n\n")));
		output_cpp( ^ ("int ", ^ (class_name, "::__FindIndex[::String inName]\n{\n")));
		PMap.iter(function _: function constructor: var name = constructor.ef_name;
				  var idx = string_of_int(constructor.ef_index);
				  output_cpp( ^ ("\tif [inName==", ^ (str(name), ^ ("] return ", ^ (idx, ";\n"))))), enum_def.e_constrs);
		output_cpp("\treturn super::__FindIndex[inName];\n");
		output_cpp("}\n\n");
		function constructor_arg_count(constructor) return {
			switch (constructor.ef_type) {
			case TFun(args, _): List.length(args);
			case _: 0;
			};
		};
		function dump_dynamic_constructor(_, constr) return {
			var count = constructor_arg_count(constr);
			if ( > (count, 0)) {
				var nargs = string_of_int(count);
				output_cpp( ^ ("STATIC_HX_DEFINE_DYNAMIC_FUNC", ^ (nargs, ^ ("[", ^ (class_name, ^ (",", ^ (keyword_remap(constr.ef_name),
				",return]\n\n")))))));
			} else {
				[];
			};
		};
		PMap.iter(dump_dynamic_constructor, enum_def.e_constrs);
		output_cpp( ^ ("int ", ^ (class_name, "::__FindArgCount[::String inName]\n{\n")));
		PMap.iter(function _: function constructor: var name = constructor.ef_name;
				  var count = string_of_int(constructor_arg_count(constructor));
				  output_cpp( ^ ("\tif [inName==", ^ (str(name), ^ ("] return ", ^ (count, ";\n"))))), enum_def.e_constrs);
		output_cpp("\treturn super::__FindArgCount[inName];\n");
		output_cpp("}\n\n");
		output_cpp( ^ ("Dynamic ", ^ (class_name, "::__Field[const ::String &inName,hx::PropertyAccess inCallProp]\n{\n")));
		function dump_constructor_test(_, constr) return {
			output_cpp( ^ ("\tif [inName==", ^ (str(constr.ef_name), ^ ("] return ", keyword_remap(constr.ef_name)))));
			if ( > (constructor_arg_count(constr), 0)) {
				output_cpp("_dyn[]");
			} else {
				[];
			};
			output_cpp(";\n");
		};
		PMap.iter(dump_constructor_test, enum_def.e_constrs);
		output_cpp("\treturn super::__Field[inName,inCallProp];\n}\n\n");
		output_cpp("static ::String sStaticFields[] = {\n");
		var sorted = List.sort(function f1: function f2: -(PMap.find(f1, enum_def.e_constrs).ef_index, PMap.find(f2, enum_def.e_constrs).ef_index), pmap_keys(enum_def.e_constrs));
		List.iter(function name: output_cpp( ^ ("\t", ^ (str(name), ",\n"))), sorted);
		output_cpp("\t::String[null[]]\n};\n\n");
		output_cpp("static void sMarkStatics[HX_MARK_PARAMS] {\n");
		PMap.iter(function _: function constructor: var name = keyword_remap(constructor.ef_name);
		switch (constructor.ef_type) {
	case TFun(_, _): [];
		case _: output_cpp( ^ ("\tHX_MARK_MEMBER_NAME[", ^ (class_name, ^ ("::", ^ (name, ^ (",\"", ^ (name, "\"];\n")))))));
		}, enum_def.e_constrs);
		output_cpp("};\n\n");
		output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n");
		output_cpp("static void sVisitStatic[HX_VISIT_PARAMS] {\n");
		output_cpp( ^ ("\tHX_VISIT_MEMBER_NAME[", ^ (class_name, "::__mClass,\"__mClass\"];\n")));
		PMap.iter(function _: function constructor: var name = keyword_remap(constructor.ef_name);
		switch (constructor.ef_type) {
	case TFun(_, _): [];
		case _: output_cpp( ^ ("\tHX_VISIT_MEMBER_NAME[", ^ (class_name, ^ ("::", ^ (name, ^ (",\"", ^ (name, "\"];\n")))))));
		}, enum_def.e_constrs);
		output_cpp("};\n");
		output_cpp("#endif\n\n");
		output_cpp("static ::String sMemberFields[] = { ::String[null[]] };\n");
		output_cpp( ^ ("hx::Class ", ^ (class_name, "::__mClass;\n\n")));
		output_cpp( ^ ("Dynamic __Create_", ^ (class_name, ^ ("[] { return new ", ^ (class_name, "; }\n\n")))));
		output_cpp( ^ ("void ", ^ (class_name, "::__register[]\n{\n")));
		var text_name = str(join_class_path(class_path, "."));
		output_cpp( ^ ("\nhx::Static[__mClass] = hx::RegisterClass[", ^ (text_name, ^ (", hx::TCanCast< ", ^ (class_name, " >,sStaticFields,sMemberFields,\n")))));
		output_cpp( ^ ("\t&__Create_", ^ (class_name, ", &__Create,\n")));
		output_cpp( ^ ("\t&super::__SGetClass[], &Create", ^ (class_name, ", sMarkStatics\n")));
		output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n    , sVisitStatic\n#endif\n");
		output_cpp("#ifdef HXCPP_SCRIPTABLE\n    , 0\n#endif\n");
		output_cpp("];\n}\n\n");
		output_cpp( ^ ("void ", ^ (class_name, "::__boot[]\n{\n")));
		switch (meta) {
		case Some(expr): var ctx = new_context(common_ctx, cpp_file, 1, file_info);
			gen_expression_tree(ctx, True, expr, "__mClass->__meta__ = ", ";\n");
		case _: [];
		};
		PMap.iter(function _: function constructor: var name = constructor.ef_name;
		switch (constructor.ef_type) {
	case TFun(_, _): [];
		case _: output_cpp( ^ ("hx::Static[", ^ (keyword_remap(name), ^ ("] = hx::CreateEnum< ", ^ (class_name, ^ (" >[",
								   ^ (str(name), ^ (",", ^ (string_of_int(constructor.ef_index), "];\n")))))))));
		}, enum_def.e_constrs);
		output_cpp("}\n\n");
		output_cpp("\n");
		gen_close_namespace(output_cpp, class_path);
		cpp_fileclose;
		var h_file = new_header_file(common_ctx, common_ctx.file, class_path);
		var super = "hx::EnumBase_obj";
		var output_h = h_filewrite;
		var def_string = join_class_path(class_path, "_");
		ctx.ctx_output = output_h;
		begin_header_file(output_h, def_string);
		List.iter(gen_forward_decl(h_file), referenced);
		gen_open_namespace(output_h, class_path);
		output_h("\n\n");
		output_h( ^ ("class ", ^ (class_name, ^ (" : public ", ^ (super, "\n")))));
		output_h( ^ ("{\n\ttypedef ", ^ (super, " super;\n")));
		output_h( ^ ("\t\ttypedef ", ^ (class_name, " OBJ_;\n")));
		output_h("\n\tpublic:\n");
		output_h( ^ ("\t\t", ^ (class_name, "[] {};\n")));
		output_h("\t\tHX_DO_ENUM_RTTI;\n");
		output_h("\t\tstatic void __boot[];\n");
		output_h("\t\tstatic void __register[];\n");
		output_h( ^ ("\t\t::String GetEnumName[ ] const { return ", ^ (str(join_class_path(class_path, ".")), "; }\n")));
		output_h( ^ ("\t\t::String __ToString[] const { return ", ^ (str( ^ (just_class_name, ".")), " + tag; }\n\n")));
		PMap.iter(function _: function constructor: var name = keyword_remap(constructor.ef_name);
				  output_h( ^ ("\t\tstatic ", ^ (remap_class_name, ^ (" ", name))));
		switch (constructor.ef_type) {
	case TFun(args, _): output_h( ^ ("[", ^ (gen_tfun_arg_list(args), "];\n")));
			output_h( ^ ("\t\tstatic Dynamic ", ^ (name, "_dyn[];\n")));
		case _: output_h(";\n");
			output_h( ^ ("\t\tstatic inline ", ^ (remap_class_name, ^ (" ", ^ (name, ^ ("_dyn[] { return ", ^ (name, "; }\n")))))));
		}, enum_def.e_constrs);
		output_h("};\n\n");
		gen_close_namespace(output_h, class_path);
		end_header_file(output_h, def_string);
		h_fileclose;
		var depend_referenced = find_referenced_types(common_ctx, TEnumDecl(enum_def), super_deps, Hashtbl.create(0), False, True, False);
		depend_referenced;
	};

	public static function list_iteri(func, in_list) return {
		var idx = ref(0);
		List.iter(function elem: func(idx.val, elem);
		idx.val = +(idx.val, 1), in_list);
	};

	public static function has_new_gc_references(class_def) return {
		switch (class_def.cl_dynamic) {
		case Some(_): True;
		case _: 	function is_gc_reference(field) return {
				&& (should_implement_field(field), && (is_data_member(field), switch (type_string(field.cf_type)) {
			case bool | int | Float: False;
			case _: True;
			}));
			};
			List.exists(is_gc_reference, class_def.cl_ordered_fields);
		};
	};

	public static function has_gc_references(class_def) return {
		|| (switch (class_def.cl_super) {
	case Some(def) if (has_gc_references(fst(def))): True;
		case _: False;
		}, has_new_gc_references(class_def));
	};

	public static function find_next_super_iteration(class_def) return {
		switch (class_def.cl_super) {
		case Some(klass, params) if (has_new_gc_references(klass)): class_string(klass, "_obj", params, True);
		case Some(klass, _): find_next_super_iteration(klass);
		case _: "";
		};
	};

	public static function has_init_field(class_def) return {
		switch (class_def.cl_init) {
		case Some(_): True;
		case _: False;
		};
	};

	public static function is_abstract_impl(class_def) return {
		switch (class_def.cl_kind) {
		case KAbstractImpl(_): True;
		case _: False;
		};
	};

	public static function variable_field(field) return {
		switch (field.cf_expr) {
		case Some({ eexpr = TFunction(function_def) }): is_dynamic_haxe_method(field);
		case _: True;
		};
	};

	public static function is_readable(class_def, field) return {
		switch (field.cf_kind) {
		case Var({ v_read = AccNever }) if (is_extern_field(field)): False;
		case Var({ v_read = AccInline }): False;
		case Var(_) if (is_abstract_impl(class_def)): False;
		case _: True;
		};
	};

	public static function is_writable(class_def, field) return {
		switch (field.cf_kind) {
		case Var({ v_write = AccNever }) if (is_extern_field(field)): False;
		case Var({ v_read = AccInline }): False;
		case Var(_) if (is_abstract_impl(class_def)): False;
		case _: True;
		};
	};

	public static function statics_except_meta(class_def) return {
		List.filter(function static: && (<>(static.cf_name, "__meta__"), <>(static.cf_name, "__rtti")), class_def.cl_ordered_statics);
	};

	public static function has_set_member_field(class_def) return {
		|| (implement_dynamic_here(class_def), var reflect_fields = List.filter(reflective(class_def), class_def.cl_ordered_fields);
		var reflect_writable = List.filter(is_writable(class_def), reflect_fields);
		List.exists(variable_field, reflect_writable));
	};

	public static function has_set_static_field(class_def) return {
		var reflect_fields = List.filter(reflective(class_def), statics_except_meta(class_def));
		var reflect_writable = List.filter(is_writable(class_def), reflect_fields);
		List.exists(variable_field, reflect_writable);
	};

	public static function has_get_fields(class_def) return {
		|| (implement_dynamic_here(class_def), 	function is_data_field(field) return {
			switch (follow(field.cf_type)) {
			case TFun(_): False;
			case _: True;
			};
		};
		List.exists(is_data_field, class_def.cl_ordered_fields));
	};

	public static function has_get_member_field(class_def) return {
		|| (implement_dynamic_here(class_def), var reflect_fields = List.filter(reflective(class_def), class_def.cl_ordered_fields);
		List.exists(is_readable(class_def), reflect_fields));
	};

	public static function has_get_static_field(class_def) return {
		var reflect_fields = List.filter(reflective(class_def), statics_except_meta(class_def));
		List.exists(is_readable(class_def), reflect_fields);
	};

	public static function has_boot_field(class_def) return {
		List.exists(has_field_init, List.filter(should_implement_field, class_def.cl_ordered_statics));
	};

	public static function is_macro(meta) return {
		Meta.has(Meta.Macro, meta);
	};

	public static function access_str(a) return {
		switch (a) {
		case AccNormal: "AccNormal";
		case AccNo: "AccNo";
		case AccNever: "AccNever";
		case AccResolve: "AccResolve";
		case AccCall: "AccCall";
		case AccInline: "AccInline";
		case AccRequire(_, _): "AccRequire";
		};
	};

	public static function generate_class_files(common_ctx, member_types, super_deps, constructor_deps, class_def, file_info,
			inScriptable) return {
		var class_path = class_def.cl_path;
		var nativeGen = has_meta_key(class_def.cl_meta, Meta.NativeGen);
		var class_name = ^ (snd(class_path), if (nativeGen) {
		"";
	} else {
		"_obj";
	});
		var dot_name = join_class_path(class_path, ".");
		var smart_class_name = snd(class_path);
		var cpp_file = new_placed_cpp_file(common_ctx, class_path);
		var output_cpp = cpp_filewrite;
		var debug = if ( || (has_meta_key(class_def.cl_meta, Meta.NoDebug), Common.defined(common_ctx, Define.NoDebug))) {
			0;
		} else {
			1;
		};
		var scriptable = && (inScriptable, !(class_def.cl_private));
		var ctx = new_context(common_ctx, cpp_file, debug, file_info);
		ctx.ctx_class_name = ^ ("::", join_class_path(class_def.cl_path, "::"));
		ctx.ctx_class_super_name = switch (class_def.cl_super) {
		case Some(klass, params): class_string(klass, "_obj", params, True);
		case _: "";
		};
		ctx.ctx_class_member_types = member_types;
		if ( > (debug, 1)) {
			print_endline( ^ ("Found class definition:", ctx.ctx_class_name));
		} else {
			[];
		};
		var ptr_name = ^ ("hx::ObjectPtr< ", ^ (class_name, " >"));
		var constructor_arg_var_list = switch (class_def.cl_constructor) {
		case Some(definition): switch (definition.cf_expr) {
			case Some({ eexpr = TFunction(function_def) }): List.map(function (v, o): (new Tuple(v.v_name, gen_arg_type_name(v.v_name,
						o, v.v_type, "__o_"))), function_def.tf_args);
			case _: switch (follow(definition.cf_type)) {
				case TFun(args, _): List.map(function (a, _, t): (new Tuple(a, (new Tuple(type_string(t), a)))), args);
				case _: [];
				};
			};
		case _: [];
		};
		var constructor_type_var_list = List.map(snd, constructor_arg_var_list);
		var constructor_var_list = List.map(snd, constructor_type_var_list);
		var constructor_type_args = String.concat(",", List.map(function (t, a): ^ (t, ^ (" ", a)), constructor_type_var_list));
		var constructor_args = String.concat(",", constructor_var_list);
		var implement_dynamic = implement_dynamic_here(class_def);
		output_cpp("#include <hxcpp.h>\n\n");
		var force_field = && (scriptable, has_get_member_field(class_def));
		var field_integer_dynamic = || (force_field, has_field_integer_lookup(class_def));
		var field_integer_numeric = || (force_field, has_field_integer_numeric_lookup(class_def));
		var all_referenced = find_referenced_types(ctx.ctx_common, TClassDecl(class_def), super_deps, constructor_deps, False, False, scriptable);
		List.iter(add_include(cpp_file), all_referenced);
		var implemented_hash = Hashtbl.create(0);
		List.iter(function imp: 	function descend_interface(interface) return {
			var imp_path = fst(interface).cl_path;
			var interface_name = ^ ("::", join_class_path_remap(imp_path, "::"));
			if (!(Hashtbl.mem(implemented_hash, interface_name))) {
				Hashtbl.add(implemented_hash, interface_name, []);
				List.iter(descend_interface, fst(interface).cl_implements);
			} else {
				[];
			};
			switch (fst(interface).cl_super) {
			case Some(interface, params): descend_interface((new Tuple(interface, params)));
			case _: [];
			};
		};
		descend_interface(imp), real_interfaces(class_def.cl_implements));
		var implemented = hash_keys(implemented_hash);
		if (scriptable) {
			output_cpp("#include <hx/Scriptable.h>\n");
		} else {
			[];
		};
		output_cpp(get_class_code(class_def, Meta.CppFileCode));
		var inc = get_meta_string_path(class_def.cl_meta, Meta.CppInclude);
		if (<>(inc, "")) {
			output_cpp( ^ ("#include \"", ^ (inc, "\"\n")));
		} else {
			[];
		};
		gen_open_namespace(output_cpp, class_path);
		output_cpp("\n");
		output_cpp(get_class_code(class_def, Meta.CppNamespaceCode));
		if ( && (!(class_def.cl_interface), !(nativeGen))) {
			output_cpp( ^ ("void ", ^ (class_name, ^ ("::__construct[", ^ (constructor_type_args, "]\n{\n")))));
			switch (class_def.cl_constructor) {
			case Some(definition): switch (definition.cf_expr) {
				case Some({ eexpr = TFunction(function_def) }):
					if (has_meta_key(definition.cf_meta, Meta.NoDebug)) {
						ctx.ctx_debug_level = 0;
					} else {
						[];
					};
					if ( > (ctx.ctx_debug_level, 0)) {
						hx_stack_push(ctx, output_cpp, dot_name, "new", function_def.tf_expr.epos);
						output_cpp("HX_STACK_THIS[this]\n");
						List.iter(function (a, (t, o)): output_cpp( ^ ("HX_STACK_ARG[", ^ (keyword_remap(o), ^ (",\"", ^ (a, "\"]\n"))))),
								  constructor_arg_var_list);
					} else {
						[];
					};
					if (has_default_values(function_def.tf_args)) {
						generate_default_values(ctx, function_def.tf_args, "__o_");
					} else {
						[];
					};
					var oldVoid = ctx.ctx_real_void;
					ctx.ctx_real_void = True;
					gen_expression_tree(ctx, False, mk_block(function_def.tf_expr), "", "");
					cpp_fileterminate_line;
					ctx.ctx_real_void = oldVoid;
					ctx.ctx_debug_level = debug;
				case _: [];
				};
			case _: [];
			};
			output_cpp("}\n\n");
			output_cpp( ^ ("Dynamic ", ^ (class_name, ^ ("::__CreateEmpty[] { return new ", ^ (class_name, "; }\n\n")))));
			output_cpp( ^ (ptr_name, ^ (" ", ^ (class_name, ^ ("::__new[", ^ (constructor_type_args, "]\n"))))));
			function create_result([]) return {
				output_cpp( ^ ("{\n\t", ^ (ptr_name, ^ (" _result_ = new ", ^ (class_name, "[];\n")))));
			};
			create_result([]);
			output_cpp( ^ ("\t_result_->__construct[", ^ (constructor_args, "];\n")));
			output_cpp("\treturn _result_;\n}\n\n");
			output_cpp( ^ ("Dynamic ", ^ (class_name, "::__Create[hx::DynamicArray inArgs]\n")));
			create_result([]);
			output_cpp( ^ ("\t_result_->__construct[", ^ (array_arg_list(constructor_var_list), "];\n")));
			output_cpp("\treturn _result_;\n}\n\n");
			if ( > (List.length(implemented), 0)) {
				output_cpp( ^ ("hx::Object *", ^ (class_name, "::__ToInterface[const hx::type_info &inType]\n{\n")));
				List.iter(function interface_name: output_cpp( ^ ("\tif [inType==typeid[ ", ^ (interface_name, ^ ("_obj]] ",
						  ^ ("return operator ", ^ (interface_name, "_obj *[];\n")))))), implemented);
				output_cpp("\treturn super::__ToInterface[inType];\n}\n\n");
				List.iter(function interface_name: output_cpp( ^ (class_name, ^ ("::operator ", ^ (interface_name, ^ ("_obj *[] { ",
						  ^ ("return new ", ^ (interface_name, ^ ("_delegate_< ", ^ (class_name, " >[this]; }\n\n"))))))))), implemented);
			} else {
				[];
			};
		} else {
			[];
		};
		switch (class_def.cl_init) {
		case Some(expression): output_cpp( ^ ("void ", ^ (class_name, "::__init__[] {\n")));
			hx_stack_push(ctx, output_cpp, dot_name, "__init__", expression.epos);
			gen_expression_tree(new_context(common_ctx, cpp_file, debug, file_info), False, mk_block(expression), "", "");
			output_cpp("}\n\n");
		case _: [];
		};
		var statics_except_meta = statics_except_meta(class_def);
		var implemented_fields = List.filter(should_implement_field, statics_except_meta);
		function dump_field_name(field) return {
			output_cpp( ^ ("\t", ^ (str(field.cf_name), ",\n")));
		};
		var implemented_instance_fields = List.filter(should_implement_field, class_def.cl_ordered_fields);
		List.iter(gen_field(ctx, class_def, class_name, smart_class_name, dot_name, False, class_def.cl_interface), class_def.cl_ordered_fields);
		List.iter(gen_field(ctx, class_def, class_name, smart_class_name, dot_name, True, class_def.cl_interface), statics_except_meta);
		output_cpp("\n");
		var override_iteration = && (!(nativeGen), has_new_gc_references(class_def));
		if ( && (!(class_def.cl_interface), !(nativeGen))) {
			output_cpp( ^ (class_name, ^ ("::", ^ (class_name, "[]\n{\n"))));
			if (implement_dynamic) {
				output_cpp("\tHX_INIT_IMPLEMENT_DYNAMIC;\n");
			} else {
				[];
			};
			List.iter(function field: var remap_name = keyword_remap(field.cf_name);
			switch (field.cf_expr) {
		case Some({ eexpr = TFunction(function_def) }):
				if (is_dynamic_haxe_method(field)) {
					output_cpp( ^ ("\t", ^ (remap_name, ^ (" = new __default_", ^ (remap_name, "[this];\n")))));
				} else {
					[];
				};
			case _: [];
			}, class_def.cl_ordered_fields);
			output_cpp("}\n\n");
			function dump_field_iterator(macro, field) return {
				if (is_data_member(field)) {
					var remap_name = keyword_remap(field.cf_name);
					output_cpp( ^ ("\t", ^ (macro, ^ ("[", ^ (remap_name, ^ (",\"", ^ (field.cf_name, "\"];\n")))))));
					switch (field.cf_kind) {
					case Var({ v_read = AccCall }) if (is_dynamic_accessor(^("get_", field.cf_name), "get", field, class_def)): var name =
						^ ("get_", field.cf_name);
						output_cpp( ^ ("\t", ^ (macro, ^ ("[", ^ (name, ^ (",", ^ ("\"", ^ (name, "\"];\n"))))))));
					case _: [];
					};
					switch (field.cf_kind) {
					case Var({ v_write = AccCall }) if (is_dynamic_accessor(^("set_", field.cf_name), "set", field, class_def)): var name =
							^ ("set_", field.cf_name);
						output_cpp( ^ ("\t", ^ (macro, ^ ("[", ^ (name, ^ (",", ^ ("\"", ^ (name, "\"];\n"))))))));
					case _: [];
					};
				} else {
					[];
				};
			};
			if (override_iteration) {
				var super_needs_iteration = find_next_super_iteration(class_def);
				output_cpp( ^ ("void ", ^ (class_name, "::__Mark[HX_MARK_PARAMS]\n{\n")));
				output_cpp( ^ ("\tHX_MARK_BEGIN_CLASS[", ^ (smart_class_name, "];\n")));
				if (implement_dynamic) {
					output_cpp("\tHX_MARK_DYNAMIC;\n");
				} else {
					[];
				};
				List.iter(dump_field_iterator("HX_MARK_MEMBER_NAME"), implemented_instance_fields);
				switch (super_needs_iteration) {
				case : [];
				case super: output_cpp( ^ ("\t", ^ (super, "::__Mark[HX_MARK_ARG];\n")));
				};
				output_cpp("\tHX_MARK_END_CLASS[];\n");
				output_cpp("}\n\n");
				output_cpp( ^ ("void ", ^ (class_name, "::__Visit[HX_VISIT_PARAMS]\n{\n")));
				if (implement_dynamic) {
					output_cpp("\tHX_VISIT_DYNAMIC;\n");
				} else {
					[];
				};
				List.iter(dump_field_iterator("HX_VISIT_MEMBER_NAME"), implemented_instance_fields);
				switch (super_needs_iteration) {
				case : [];
				case super: output_cpp( ^ ("\t", ^ (super, "::__Visit[HX_VISIT_ARG];\n")));
				};
				output_cpp("}\n\n");
			} else {
				[];
			};
			var reflect_member_fields = List.filter(reflective(class_def), class_def.cl_ordered_fields);
			var reflect_member_readable = List.filter(is_readable(class_def), reflect_member_fields);
			var reflect_member_writable = List.filter(is_writable(class_def), reflect_member_fields);
			var reflect_write_member_variables = List.filter(variable_field, reflect_member_writable);
			var reflect_static_fields = List.filter(reflective(class_def), statics_except_meta);
			var reflect_static_readable = List.filter(is_readable(class_def), reflect_static_fields);
			var reflect_static_writable = List.filter(is_writable(class_def), reflect_static_fields);
			var reflect_write_static_variables = List.filter(variable_field, reflect_static_writable);
			function dump_quick_field_test(fields) return {
				if ( > (List.length(fields), 0)) {
					function len(Tuple(_, l, _)) return {
						l;
					};
					var sfields = List.sort(function f1: function f2: -(len(f1), len(f2)), fields);
					var len_case = ref(-1);
					output_cpp("\tswitch[inName.length] {\n");
					List.iter(function (field, l, result):
					if (<>(l, len_case.val)) {
					if ( >= (len_case.val, 0)) {
							output_cpp("\t\tbreak;\n");
						} else {
							[];
						};
						output_cpp( ^ ("\tcase ", ^ (string_of_int(l), ":\n")));
						len_case.val = l;
					} else {
						[];
					};
					output_cpp( ^ ("\t\tif [HX_FIELD_EQ[inName,\"", ^ (Ast.s_escape(field), ^ ("\"] ] { ", ^ (result, " }\n"))))), sfields);
					output_cpp("\t}\n");
				} else {
					[];
				};
			};
			function checkPropCall(field) return {
				if ( || (has_meta_key(class_def.cl_meta, Meta.NativeProperty), || (has_meta_key(field.cf_meta, Meta.NativeProperty), Common.defined(common_ctx, Define.ForceNativeProperty)))) {
					"inCallProp != hx::paccNever";
				} else {
					"inCallProp == hx::paccAlways";
				};
			};
			if (has_get_member_field(class_def)) {
				output_cpp( ^ ("Dynamic ", ^ (class_name, "::__Field[const ::String &inName,hx::PropertyAccess inCallProp]\n{\n")));
				var get_field_dat = List.map(function f: (new Tuple(f.cf_name, String.length(f.cf_name), ^ (switch (f.cf_kind) {
			case Var({ v_read = AccCall }) if (is_extern_field(f)): ^ ("if [", ^ (checkPropCall(f), ^ ("] return ",
							^ (keyword_remap( ^ ("get_", f.cf_name)), "[]"))));
				case Var({ v_read = AccCall }): ^ ("return ", ^ (checkPropCall(f), ^ (" ? ", ^ (keyword_remap( ^ ("get_", f.cf_name)),
					^ ("[] : ", ^ (keyword_remap(f.cf_name), if (variable_field(f)) {
					"";
				} else {
					"_dyn[]";
				}))))));
				case _: ^ ("return ", ^ (keyword_remap(f.cf_name), if (variable_field(f)) {
					"";
				} else {
					"_dyn[]";
				}));
				}, ";"))));
				dump_quick_field_test(get_field_dat(reflect_member_readable));
				if (implement_dynamic) {
					output_cpp("\tHX_CHECK_DYNAMIC_GET_FIELD[inName];\n");
				} else {
					[];
				};
				output_cpp("\treturn super::__Field[inName,inCallProp];\n}\n\n");
				if ( || (field_integer_numeric, field_integer_dynamic)) {
					function dump_static_ids(field) return {
						var remap_name = keyword_remap(field.cf_name);
						output_cpp( ^ ("static int __id_", ^ (remap_name, ^ (" = __hxcpp_field_to_id[\"", ^ (field.cf_name, "\"];\n")))));
					};
					List.iter(dump_static_ids, reflect_member_readable);
					output_cpp("\n\n");
					function output_ifield(return_type, function_name, all_fields) return {
						output_cpp( ^ (return_type, ^ (" ", ^ (class_name, ^ ("::", ^ (function_name, "[int inFieldID]\n{\n"))))));
						function dump_field_test(f) return {
							var remap_name = keyword_remap(f.cf_name);
							output_cpp( ^ ("\tif [inFieldID==__id_", ^ (remap_name, ^ ("] return ", ^ (if ( = (return_type, "Float")) {
							"hx::ToDouble[ ";
						} else {
							"";
						}, ^ (switch (f.cf_kind) {
						case Var({ v_read = AccCall }): ^ (keyword_remap( ^ ("get_", f.cf_name)), "[]");
							case _: ^ (remap_name, if (variable_field(f)) {
								"";
							} else {
								"_dyn[]";
							});
							}, ^ (if ( = (return_type, "Float")) {
							" ] ";
						} else {
							"";
						}, ";\n")))))));
						};
						List.iter(dump_field_test, List.filter(function f: || (all_fields, is_numeric_field(f)), reflect_member_readable));
						if (implement_dynamic) {
							output_cpp("\tHX_CHECK_DYNAMIC_GET_INT_FIELD[inFieldID];\n");
						} else {
							[];
						};
						output_cpp( ^ ("\treturn super::", ^ (function_name, "[inFieldID];\n}\n\n")));
					};
					if (field_integer_dynamic) {
						output_ifield("Dynamic", "__IField", True);
					} else {
						[];
					};
					if (field_integer_numeric) {
						output_ifield("double", "__INumField", False);
					} else {
						[];
					};
				} else {
					[];
				};
			} else {
				[];
			};
			if (has_get_static_field(class_def)) {
				output_cpp( ^ ("bool ", ^ (class_name,
										   "::__GetStatic[const ::String &inName, Dynamic &outValue, hx::PropertyAccess inCallProp]\n{\n")));
				var get_field_dat = List.map(function f: (new Tuple(f.cf_name, String.length(f.cf_name), switch (f.cf_kind) {
			case Var({ v_read = AccCall }) if (is_extern_field(f)): ^ ("if [", ^ (checkPropCall(f), ^ ("] { outValue = ",
							^ (keyword_remap( ^ ("get_", f.cf_name)), "[]; return true; }"))));
				case Var({ v_read = AccCall }): ^ ("outValue = ", ^ (checkPropCall(f), ^ (" ? ", ^ (keyword_remap( ^ ("get_", f.cf_name)),
					^ ("[] : ", ^ ( ^ (keyword_remap(f.cf_name), if (variable_field(f)) {
					"";
				} else {
					"_dyn[]";
				}), "; return true;"))))));
				case _: ^ ("outValue = ", ^ (keyword_remap(f.cf_name), ^ (if (variable_field(f)) {
					"";
				} else {
					"_dyn[]";
				}, "; return true;")));
				})));
				dump_quick_field_test(get_field_dat(reflect_static_readable));
				output_cpp("\treturn false;\n}\n\n");
			} else {
				[];
			};
			if (has_set_member_field(class_def)) {
				output_cpp( ^ ("Dynamic ", ^ (class_name,
											  "::__SetField[const ::String &inName,const Dynamic &inValue,hx::PropertyAccess inCallProp]\n{\n")));
				var set_field_dat = List.map(function f: var default_action = ^ (keyword_remap(f.cf_name), ^ ("=inValue.Cast< ",
											 ^ (type_string(f.cf_type), ^ (" >[];", " return inValue;"))));
				(new Tuple(f.cf_name, String.length(f.cf_name), switch (f.cf_kind) {
			case Var({ v_write = AccCall }): ^ ("if [", ^ (checkPropCall(f), ^ ("] return ", ^ (keyword_remap( ^ ("set_", f.cf_name)),
					^ ("[inValue];", if (is_extern_field(f)) {
					"";
				} else {
					default_action;
				})))));
				case _: default_action;
				})));
				dump_quick_field_test(set_field_dat(reflect_write_member_variables));
				if (implement_dynamic) {
					output_cpp("\ttry { return super::__SetField[inName,inValue,inCallProp]; }\n");
					output_cpp("\tcatch[Dynamic e] { HX_DYNAMIC_SET_FIELD[inName,inValue]; }\n");
					output_cpp("\treturn inValue;\n}\n\n");
				} else {
					output_cpp("\treturn super::__SetField[inName,inValue,inCallProp];\n}\n\n");
				};
			} else {
				[];
			};
			if (has_set_static_field(class_def)) {
				output_cpp( ^ ("bool ", ^ (class_name,
										   "::__SetStatic[const ::String &inName,Dynamic &ioValue,hx::PropertyAccess inCallProp]\n{\n")));
				var set_field_dat = List.map(function f: var default_action = ^ (keyword_remap(f.cf_name), ^ ("=ioValue.Cast< ",
											 ^ (type_string(f.cf_type), " >[]; return true;")));
				(new Tuple(f.cf_name, String.length(f.cf_name), switch (f.cf_kind) {
			case Var({ v_write = AccCall }): ^ ("if [", ^ (checkPropCall(f), ^ ("]  ioValue = ", ^ (keyword_remap( ^ ("set_",
					f.cf_name)), ^ ("[ioValue];", if (is_extern_field(f)) {
					"";
				} else {
					^ (" else ", default_action);
					})))));
				case _: default_action;
				})));
				dump_quick_field_test(set_field_dat(reflect_write_static_variables));
				output_cpp("\treturn false;\n}\n\n");
			} else {
				[];
			};
			if (has_get_fields(class_def)) {
				function append_field(field) return {
					output_cpp( ^ ("\toutFields->push[", ^ (str(field.cf_name), "];\n")));
				};
				function is_data_field(field) return {
					switch (follow(field.cf_type)) {
					case TFun(_): False;
					case _: True;
					};
				};
				output_cpp( ^ ("void ", ^ (class_name, "::__GetFields[Array< ::String> &outFields]\n{\n")));
				List.iter(append_field, List.filter(is_data_field, class_def.cl_ordered_fields));
				if (implement_dynamic) {
					output_cpp("\tHX_APPEND_DYNAMIC_FIELDS[outFields];\n");
				} else {
					[];
				};
				output_cpp("\tsuper::__GetFields[outFields];\n");
				output_cpp("};\n\n");
			} else {
				[];
			};
			function storage(field) return {
				switch (type_string(field.cf_type)) {
				case bool: "hx::fsBool";
				case int: "hx::fsInt";
				case Float: "hx::fsFloat";
				case ::String: "hx::fsString";
				case str: ^ ("hx::fsObject", ^ (" /*", ^ (str, "*/ ")));
				};
			};
			function dump_member_storage(field) return {
				output_cpp( ^ ("\t{", ^ (storage(field), ^ (",[int]offsetof[", ^ (class_name, ^ (",", ^ (keyword_remap(field.cf_name), ^ ("],", ^ (str(field.cf_name), "},\n")))))))));
			};
			function dump_static_storage(field) return {
				output_cpp( ^ ("\t{", ^ (storage(field), ^ (",[void *] &", ^ (class_name, ^ ("::", ^ (keyword_remap(field.cf_name), ^ (",", ^ (str(field.cf_name), "},\n")))))))));
			};
			output_cpp("#if HXCPP_SCRIPTABLE\n");
			var stored_fields = List.filter(is_data_member, implemented_instance_fields);
			if ( > (List.length(stored_fields), 0)) {
				output_cpp("static hx::StorageInfo sMemberStorageInfo[] = {\n");
				List.iter(dump_member_storage, stored_fields);
				output_cpp("\t{ hx::fsUnknown, 0, null[]}\n};\n");
			} else {
				output_cpp("static hx::StorageInfo *sMemberStorageInfo = 0;\n");
			};
			var stored_statics = List.filter(is_data_member, implemented_fields);
			if ( > (List.length(stored_statics), 0)) {
				output_cpp("static hx::StaticInfo sStaticStorageInfo[] = {\n");
				List.iter(dump_static_storage, stored_statics);
				output_cpp("\t{ hx::fsUnknown, 0, null[]}\n};\n");
			} else {
				output_cpp("static hx::StaticInfo *sStaticStorageInfo = 0;\n");
			};
			output_cpp("#endif\n\n");
		} else {
			[];
		};
		var reflective_members = List.filter(reflective(class_def), implemented_instance_fields);
		var sMemberFields = if ( > (List.length(reflective_members), 0)) {
			output_cpp("static ::String sMemberFields[] = {\n");
			List.iter(dump_field_name, reflective_members);
			output_cpp("\t::String[null[]] };\n\n");
			"sMemberFields";
		} else {
			"0 /* sMemberFields */";
		};
		if (!(nativeGen)) {
			output_cpp("static void sMarkStatics[HX_MARK_PARAMS] {\n");
			output_cpp( ^ ("\tHX_MARK_MEMBER_NAME[", ^ (class_name, "::__mClass,\"__mClass\"];\n")));
			List.iter(function field:
			if (is_data_member(field)) {
			output_cpp( ^ ("\tHX_MARK_MEMBER_NAME[", ^ (class_name, ^ ("::", ^ (keyword_remap(field.cf_name), ^ (",\"",
						   ^ (field.cf_name, "\"];\n")))))));
			} else {
				[];
			}, implemented_fields);
			output_cpp("};\n\n");
			output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n");
			output_cpp("static void sVisitStatics[HX_VISIT_PARAMS] {\n");
			output_cpp( ^ ("\tHX_VISIT_MEMBER_NAME[", ^ (class_name, "::__mClass,\"__mClass\"];\n")));
			List.iter(function field:
			if (is_data_member(field)) {
			output_cpp( ^ ("\tHX_VISIT_MEMBER_NAME[", ^ (class_name, ^ ("::", ^ (keyword_remap(field.cf_name), ^ (",\"",
						   ^ (field.cf_name, "\"];\n")))))));
			} else {
				[];
			}, implemented_fields);
			output_cpp("};\n\n");
			output_cpp("#endif\n\n");
		} else {
			[];
		};
		function script_type(t, optional) return {
			if (optional) {
				"Object";
			} else {
				switch (type_string(t)) {
				case bool: "Int";
				case int: "Int";
				case Float: "Float";
				case ::String: "String";
				case Null: "Void";
				case Void: "Void";
				case _: "Object";
				};
			};
		};
		function script_signature(t, optional) return {
			switch (script_type(t, optional)) {
			case Bool: "b";
			case Int: "i";
			case Float: "f";
			case String: "s";
			case Void: "v";
			case _: "o";
			};
		};
		function script_size_type(t, optional) return {
			switch (script_type(t, optional)) {
			case Object: "void *";
			case x: x;
			};
		};
		function generate_script_function(isStatic, field, scriptName, callName) return {
			switch (follow(field.cf_type)) {
			case TFun(args, return_type): output_cpp( ^ ("\nstatic void ", ^ (scriptName, "[hx::CppiaCtx *ctx] {\n")));
				var ret = script_signature(return_type, False);
				if (<>(ret, "v")) {
					output_cpp( ^ ("ctx->return", ^ (script_type(return_type, False), "[")));
				} else {
					[];
				};
				if (isStatic) {
					output_cpp( ^ (class_name, ^ ("::", ^ (callName, "["))));
				} else {
					output_cpp( ^ ("[[", ^ (class_name, ^ ("*]ctx->getThis[]]->", ^ (callName, "[")))));
				};
				var Tuple(signature, _, _) = List.fold_left(function (signature, sep, size): function (_, opt, t): output_cpp( ^ (sep,
											 ^ ("ctx->get", ^ (script_type(t, opt), ^ ("[", ^ (size, "]"))))));
											 (new Tuple( ^ (signature, script_signature(t, opt)), ",", ^ (size, ^ ("+sizeof[", ^ (script_size_type(t, opt), "]"))))),
											 (new Tuple(ret, "", "sizeof[void*]")), args);
				output_cpp("]");
				if (<>(ret, "v")) {
					output_cpp("]");
				} else {
					[];
				};
				output_cpp(";\n}\n");
				signature;
			case _: "";
			};
		};
		if ( && (scriptable, !(nativeGen))) {
			function dump_script_field(idx, Tuple(field, f_args, return_t)) return {
				var args = if (class_def.cl_interface) {
					gen_tfun_interface_arg_list(f_args);
				} else {
					gen_tfun_arg_list(f_args);
				};
				var names = List.map(function (n, _, _): keyword_remap(n), f_args);
				var return_type = type_string(return_t);
				var ret = if ( = (return_type, "Void")) {
					" ";
				} else {
					"return ";
				};
				var name = keyword_remap(field.cf_name);
				var vtable = ^ ("__scriptVTable[", ^ (string_of_int(+(idx, 1)), "] "));
				var args_varray = List.fold_left(function l: function n: ^ (l, ^ (".Add[", ^ (n, "]"))), "Array<Dynamic>[]", names);
				output_cpp( ^ ("	", ^ (return_type, ^ (" ", ^ (name, ^ ("[ ", ^ (args, " ] { ")))))));
				output_cpp( ^ ("\n\tif [", ^ (vtable, "] {\n")));
				output_cpp("\t\thx::CppiaCtx *__ctx = hx::CppiaCtx::getCurrent[];\n");
				output_cpp("\t\thx::AutoStack __as[__ctx];\n");
				output_cpp( ^ ("\t\t__ctx->pushObject[", ^ (if (class_def.cl_interface) {
				"mDelegate.mPtr";
			} else {
				"this";
			}, "];\n")));
				List.iter(function (name, opt, t): output_cpp( ^ ("\t\t__ctx->push", ^ (script_type(t, opt), ^ ("[", ^ (keyword_remap(name), "];\n"))))), f_args);
				output_cpp( ^ ("\t\t", ^ (ret, ^ ("__ctx->run", ^ (script_type(return_t, False), ^ ("[", ^ (vtable, "];\n")))))));
				output_cpp( ^ ("\t}  else ", ret));
				if (class_def.cl_interface) {
					output_cpp( ^ (" mDelegate->__Field[HX_CSTRING[\"", ^ (field.cf_name, "\"], hx::paccNever]")));
					if ( <= (List.length(names), 5)) {
						output_cpp( ^ ("->__run[", ^ (String.concat(",", names), "];")));
					} else {
						output_cpp( ^ ("->__Run[", ^ (args_varray, "];")));
					};
				} else {
					output_cpp( ^ (class_name, ^ ("::", ^ (name, ^ ("[", ^ (String.concat(",", names), "];"))))));
				};
				output_cpp("return null[]; }\n");
				if (class_def.cl_interface) {
					output_cpp( ^ ("	Dynamic ", ^ (name, ^ ("_dyn[] { return mDelegate->__Field[HX_CSTRING[\"", ^ (field.cf_name,
													  "\"], hx::paccNever]; }\n\n")))));
				} else {
					[];
				};
			};
			function not_toString(Tuple(field, args, _)) return {
				|| (<>(field.cf_name, "toString"), class_def.cl_interface);
			};
			var functions = List.filter(not_toString, all_virtual_functions(class_def));
			var new_sctipt_functions = List.filter(function (f, _, _): !(is_override(class_def, f.cf_name)), functions);
			var sctipt_name = ^ (class_name, "__scriptable");
			output_cpp( ^ ("class ", ^ (sctipt_name, ^ (" : public ", ^ (class_name, " {\n")))));
			output_cpp( ^ ("   typedef ", ^ (sctipt_name, " __ME;\n")));
			output_cpp( ^ ("   typedef ", ^ (class_name, " super;\n")));
			var has_funky_toString =
				|| (List.exists(function f: = (f.cf_name, "toString"), class_def.cl_ordered_statics), List.exists(function f:
						&& ( = (f.cf_name, "toString"), <>(field_arg_count(f), 0)), class_def.cl_ordered_fields));
			var super_string = if (has_funky_toString) {
				^ (class_name, "::super");
			} else {
				class_name;
			};
			output_cpp( ^ ("   typedef ", ^ (super_string, " __superString;\n")));
			if (class_def.cl_interface) {
				output_cpp("   HX_DEFINE_SCRIPTABLE_INTERFACE\n");
			} else {
				output_cpp( ^ ("   HX_DEFINE_SCRIPTABLE[HX_ARR_LIST", ^ (string_of_int(List.length(constructor_var_list)), "]\n")));
				if (!(implement_dynamic)) {
					output_cpp("\tHX_DEFINE_SCRIPTABLE_DYNAMIC;\n");
				} else {
					[];
				};
			};
			list_iteri(dump_script_field, functions);
			output_cpp("};\n\n");
			if ( > (List.length(new_sctipt_functions), 0)) {
				var sigs = Hashtbl.create(0);
				List.iter(function (f, _, _): var s = generate_script_function(False, f, ^ ("__s_", f.cf_name), keyword_remap(f.cf_name));
						  Hashtbl.add(sigs, f.cf_name, s), new_sctipt_functions);
				output_cpp("static hx::ScriptNamedFunction __scriptableFunctions[] = {\n");
				List.iter(function (f, _, _): var s = try {
					Hashtbl.find(sigs, f.cf_name);
				} catch (e: Not_found) {
					"v";
				};
				output_cpp( ^ ("  hx::ScriptNamedFunction[\"", ^ (f.cf_name, ^ ("\",__s_", ^ (f.cf_name, ^ (",\"", ^ (s, "\"],\n"))))))),
				new_sctipt_functions);
				output_cpp("  hx::ScriptNamedFunction[0,0,0] };\n");
			} else {
				output_cpp("static hx::ScriptNamedFunction *__scriptableFunctions = 0;\n");
			};
		} else {
			[];
		};
		var class_name_text = join_class_path(class_path, ".");
		if ( && (!(class_def.cl_interface), !(nativeGen))) {
			output_cpp( ^ ("hx::Class ", ^ (class_name, "::__mClass;\n\n")));
			if (scriptable) {
				switch (class_def.cl_constructor) {
				case Some(field): var signature = generate_script_function(False, field, "__script_construct_func", "__construct");
					output_cpp( ^ ("hx::ScriptFunction ", ^ (class_name, ^ ("::__script_construct[__script_construct_func,\"", ^ (signature,
								   "\"];\n")))));
				case _: output_cpp( ^ ("hx::ScriptFunction ", ^ (class_name, "::__script_construct[0,0];\n")));
				};
			} else {
				[];
			};
			var reflective_statics = List.filter(reflective(class_def), implemented_fields);
			var sStaticFields = if ( > (List.length(reflective_statics), 0)) {
				output_cpp("static ::String sStaticFields[] = {\n");
				List.iter(dump_field_name, reflective_statics);
				output_cpp("\t::String[null[]]\n};\n\n");
				"sStaticFields";
			} else {
				"0 /* sStaticFields */";
			};
			output_cpp( ^ ("void ", ^ (class_name, "::__register[]\n{\n")));
			output_cpp("\thx::Static[__mClass] = new hx::Class_obj[];\n");
			output_cpp( ^ ("\t__mClass->mName = ", ^ (str(class_name_text), ";\n")));
			output_cpp("\t__mClass->mSuper = &super::__SGetClass[];\n");
			output_cpp("\t__mClass->mConstructEmpty = &__CreateEmpty;\n");
			output_cpp("\t__mClass->mConstructArgs = &__Create;\n");
			output_cpp( ^ ("\t__mClass->mGetStaticField = &", if (has_get_static_field(class_def)) {
			^ (class_name, "::__GetStatic;\n");
			} else {
				"hx::Class_obj::GetNoStaticField;\n";
			}));
			output_cpp( ^ ("\t__mClass->mSetStaticField = &", if (has_set_static_field(class_def)) {
			^ (class_name, "::__SetStatic;\n");
			} else {
				"hx::Class_obj::SetNoStaticField;\n";
			}));
			output_cpp("\t__mClass->mMarkFunc = sMarkStatics;\n");
			output_cpp( ^ ("\t__mClass->mStatics = hx::Class_obj::dupFunctions[", ^ (sStaticFields, "];\n")));
			output_cpp( ^ ("\t__mClass->mMembers = hx::Class_obj::dupFunctions[", ^ (sMemberFields, "];\n")));
			output_cpp( ^ ("\t__mClass->mCanCast = hx::TCanCast< ", ^ (class_name, " >;\n")));
			output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = sVisitStatics;\n#endif\n");
			output_cpp("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mMemberStorageInfo = sMemberStorageInfo;\n#endif\n");
			output_cpp("#ifdef HXCPP_SCRIPTABLE\n\t__mClass->mStaticStorageInfo = sStaticStorageInfo;\n#endif\n");
			output_cpp("\thx::RegisterClass[__mClass->mName, __mClass];\n");
			if (scriptable) {
				output_cpp( ^ ("  HX_SCRIPTABLE_REGISTER_CLASS[\"", ^ (class_name_text, ^ ("\",", ^ (class_name, "];\n")))));
			} else {
				[];
			};
			output_cpp("}\n\n");
		} else {
			if (!(nativeGen)) {
				output_cpp( ^ ("hx::Class ", ^ (class_name, "::__mClass;\n\n")));
				output_cpp( ^ ("void ", ^ (class_name, "::__register[]\n{\n")));
				output_cpp("\thx::Static[__mClass] = new hx::Class_obj[];\n");
				output_cpp( ^ ("\t__mClass->mName = ", ^ (str(class_name_text), ";\n")));
				output_cpp("\t__mClass->mSuper = &super::__SGetClass[];\n");
				output_cpp("\t__mClass->mMarkFunc = sMarkStatics;\n");
				output_cpp( ^ ("\t__mClass->mMembers = hx::Class_obj::dupFunctions[", ^ (sMemberFields, "];\n")));
				output_cpp( ^ ("\t__mClass->mCanCast = hx::TCanCast< ", ^ (class_name, " >;\n")));
				output_cpp("#ifdef HXCPP_VISIT_ALLOCS\n\t__mClass->mVisitFunc = sVisitStatics;\n#endif\n");
				output_cpp("\thx::RegisterClass[__mClass->mName, __mClass];\n");
				if (scriptable) {
					output_cpp( ^ ("  HX_SCRIPTABLE_REGISTER_INTERFACE[\"", ^ (class_name_text, ^ ("\",", ^ (class_name, "];\n")))));
				} else {
					[];
				};
				output_cpp("}\n\n");
			} else {
				[];
			};
		};
		if (has_boot_field(class_def)) {
			output_cpp( ^ ("void ", ^ (class_name, "::__boot[]\n{\n")));
			List.iter(gen_field_init(ctx), List.filter(should_implement_field, class_def.cl_ordered_statics));
			output_cpp("}\n\n");
		} else {
			[];
		};
		gen_close_namespace(output_cpp, class_path);
		cpp_fileclose;
		var h_file = new_header_file(common_ctx, common_ctx.file, class_path);
		var super = switch (class_def.cl_super) {
		case Some(klass, params): class_string(klass, "_obj", params, True);
		case _ if (nativeGen): "";
		case _: if (class_def.cl_interface) {
				"hx::Interface";
			} else {
				"hx::Object";
			};
		};
		var output_h = h_filewrite;
		var def_string = join_class_path(class_path, "_");
		ctx.ctx_output = output_h;
		begin_header_file(output_h, def_string);
		switch (class_def.cl_super) {
		case Some(super): var super_path = fst(super).cl_path;
			h_fileadd_include(super_path);
		case _: [];
		};
		List.iter(function imp: h_fileadd_include(fst(imp).cl_path), real_interfaces(class_def.cl_implements));
		var referenced = find_referenced_types(ctx.ctx_common, TClassDecl(class_def), super_deps, Hashtbl.create(0), True, False, scriptable);
		List.iter(gen_forward_decl(h_file), referenced);
		output_h(get_class_code(class_def, Meta.HeaderCode));
		var inc = get_meta_string_path(class_def.cl_meta, Meta.HeaderInclude);
		if (<>(inc, "")) {
			output_h( ^ ("#include \"", ^ (inc, "\"\n")));
		} else {
			[];
		};
		gen_open_namespace(output_h, class_path);
		output_h("\n\n");
		output_h(get_class_code(class_def, Meta.HeaderNamespaceCode));
		var extern_class = Common.defined(common_ctx, Define.DllExport);
		var attribs = ^ ("HXCPP_", ^ (if (extern_class) {
		"EXTERN_";
	} else {
		"";
	}, "CLASS_ATTRIBUTES"));
		if ( = (super, "")) {
			output_h( ^ ("class ", ^ (attribs, ^ (" ", class_name))));
			output_h("\n{\n\tpublic:\n");
		} else {
			output_h( ^ ("class ", ^ (attribs, ^ (" ", ^ (class_name, ^ (" : public ", super))))));
			output_h("\n{\n\tpublic:\n");
			output_h( ^ ("\t\ttypedef ", ^ (super, " super;\n")));
			output_h( ^ ("\t\ttypedef ", ^ (class_name, " OBJ_;\n")));
		};
		if ( && (!(class_def.cl_interface), !(nativeGen))) {
			output_h( ^ ("\t\t", ^ (class_name, "[];\n")));
			output_h( ^ ("\t\tvoid __construct[", ^ (constructor_type_args, "];\n")));
			output_h("\n\tpublic:\n");
			var new_arg = if (has_gc_references(class_def)) {
				"true";
			} else {
				"false";
			};
			output_h( ^ ("\t\tinline void *operator new[size_t inSize, bool inContainer=", ^ (new_arg, ^ (",const char *inName=",
						 ^ (const_char_star(class_name_text), "]\n")))));
			output_h("\t\t\t{ return hx::Object::operator new[inSize,inContainer,inName]; }\n");
			output_h( ^ ("\t\tstatic ", ^ (ptr_name, ^ (" __new[", ^ (constructor_type_args, "];\n")))));
			output_h("\t\tstatic Dynamic __CreateEmpty[];\n");
			output_h("\t\tstatic Dynamic __Create[hx::DynamicArray inArgs];\n");
			if (scriptable) {
				output_h("\t\tstatic hx::ScriptFunction __script_construct;\n");
			} else {
				[];
			};
			output_h( ^ ("\t\t//~", ^ (class_name, "[];\n\n")));
			output_h("\t\tHX_DO_RTTI_ALL;\n");
			if (has_get_member_field(class_def)) {
				output_h("\t\tDynamic __Field[const ::String &inString, hx::PropertyAccess inCallProp];\n");
			} else {
				[];
			};
			if (has_get_static_field(class_def)) {
				output_h("\t\tstatic bool __GetStatic[const ::String &inString, Dynamic &outValue, hx::PropertyAccess inCallProp];\n");
			} else {
				[];
			};
			if (has_set_member_field(class_def)) {
				output_h("\t\tDynamic __SetField[const ::String &inString,const Dynamic &inValue, hx::PropertyAccess inCallProp];\n");
			} else {
				[];
			};
			if (has_set_static_field(class_def)) {
				output_h("\t\tstatic bool __SetStatic[const ::String &inString, Dynamic &ioValue, hx::PropertyAccess inCallProp];\n");
			} else {
				[];
			};
			if (has_get_fields(class_def)) {
				output_h("\t\tvoid __GetFields[Array< ::String> &outFields];\n");
			} else {
				[];
			};
			if (field_integer_dynamic) {
				output_h("\t\tDynamic __IField[int inFieldID];\n");
			} else {
				[];
			};
			if (field_integer_numeric) {
				output_h("\t\tdouble __INumField[int inFieldID];\n");
			} else {
				[];
			};
			if (implement_dynamic) {
				output_h("\t\tHX_DECLARE_IMPLEMENT_DYNAMIC;\n");
			} else {
				[];
			};
			output_h("\t\tstatic void __register[];\n");
			if (override_iteration) {
				output_h("\t\tvoid __Mark[HX_MARK_PARAMS];\n");
				output_h("\t\tvoid __Visit[HX_VISIT_PARAMS];\n");
			} else {
				[];
			};
			if ( > (List.length(implemented), 0)) {
				output_h("\t\thx::Object *__ToInterface[const hx::type_info &inType];\n");
				List.iter(function interface_name: output_h( ^ ("\t\toperator ", ^ (interface_name, "_obj *[];\n"))), implemented);
			} else {
				[];
			};
			if (has_init_field(class_def)) {
				output_h("\t\tstatic void __init__[];\n\n");
			} else {
				[];
			};
			output_h( ^ ("\t\t::String __ToString[] const { return ", ^ (str(smart_class_name), "; }\n\n")));
		} else {
			if (!(nativeGen)) {
				output_h("\t\tHX_DO_INTERFACE_RTTI;\n");
			} else {
				[];
			};
		};
		if (has_boot_field(class_def)) {
			output_h("\t\tstatic void __boot[];\n");
		} else {
			[];
		};
		switch (class_def.cl_array_access) {
		case Some(t): output_h( ^ ("\t\ttypedef ", ^ (type_string(t), " __array_access;\n")));
		case _: [];
		};
		List.iter(gen_member_def(ctx, class_def, True, class_def.cl_interface), List.filter(should_implement_field, class_def.cl_ordered_statics));
		if (class_def.cl_interface) {
			var dumped = ref(PMap.empty);
			function dump_def(interface) return {
				List.iter(function field:
				try {
					ignore(PMap.find(field.cf_name, dumped.val));
				} catch (e: Not_found) {
					dumped.val = PMap.add(field.cf_name, True, dumped.val);
					gen_member_def(ctx, interface, False, True, field);
				}, interface.cl_ordered_fields);
				List.iter(function impl: dump_def(fst(impl)), real_interfaces(interface.cl_implements));
			};
			dump_def(class_def);
			List.iter(function impl: dump_def(fst(impl)), real_interfaces(class_def.cl_implements));
		} else {
			List.iter(gen_member_def(ctx, class_def, False, False), List.filter(should_implement_field, class_def.cl_ordered_fields));
		};
		output_h(get_class_code(class_def, Meta.HeaderClassCode));
		output_h("};\n\n");
		if ( && (class_def.cl_interface, !(nativeGen))) {
			output_h("\n\n");
			output_h("template<typename IMPL>\n");
			output_h( ^ ("class ", ^ (smart_class_name, ^ ("_delegate_ : public ", ^ (class_name, "\n")))));
			output_h("{\n\tprotected:\n");
			output_h("\t\tIMPL *mDelegate;\n");
			output_h("\tpublic:\n");
			output_h( ^ ("\t\t", ^ (smart_class_name, "_delegate_[IMPL *inDelegate] : mDelegate[inDelegate] {}\n")));
			output_h("\t\thx::Object *__GetRealObject[] { return mDelegate; }\n");
			output_h("\t\tvoid __Visit[HX_VISIT_PARAMS] { HX_VISIT_OBJECT[mDelegate]; }\n");
			var dumped = ref(PMap.empty);
			function dump_delegate(interface) return {
				List.iter(function field:
				try {
					ignore(PMap.find(field.cf_name, dumped.val));
				} catch (e: Not_found) {
					dumped.val = PMap.add(field.cf_name, True, dumped.val);
					switch ((new Tuple(follow(field.cf_type), field.cf_kind))) {
					case (_, Method(MethDynamic)): [];
					case (TFun(args, return_type), Method(_)): var remap_name = keyword_remap(field.cf_name);
						output_h( ^ ("		", ^ (type_string(return_type), ^ (" ", ^ (remap_name, "[ ")))));
						output_h(gen_tfun_interface_arg_list(args));
						output_h( ^ ("] { return mDelegate->", ^ (remap_name, "[")));
						output_h(String.concat(",", List.map(function (name, opt, typ): keyword_remap(name), args)));
						output_h("];}\n");
						if (reflective(interface, field)) {
							output_h( ^ ("		Dynamic ", ^ (remap_name, ^ ("_dyn[] { return mDelegate->", ^ (remap_name, "_dyn[];}\n")))));
						} else {
							[];
						};
					case _: [];
					};
				}, interface.cl_ordered_fields);
				switch (interface.cl_super) {
				case Some(super): dump_delegate(fst(super));
				case _: [];
					List.iter(function impl: dump_delegate(fst(impl)), real_interfaces(interface.cl_implements));
				};
			};
			dump_delegate(class_def);
			List.iter(function impl: dump_delegate(fst(impl)), real_interfaces(class_def.cl_implements));
			output_h("};\n\n");
		} else {
			[];
		};
		gen_close_namespace(output_h, class_path);
		end_header_file(output_h, def_string);
		h_fileclose;
		var depend_referenced = find_referenced_types(ctx.ctx_common, TClassDecl(class_def), super_deps, constructor_deps, False, True, False);
		depend_referenced;
	};

	public static function write_resources(common_ctx) return {
		var idx = ref(0);
		Hashtbl.iter(function _: function data: var id = ^ ("__res_", string_of_int(idx.val));
		var resource_file = new_cpp_file(common_ctx, common_ctx.file, (new Tuple(::("resources", []), id)));
		resource_filewrite("namespace hx {\n");
		resource_filewrite_i( ^ ("unsigned char ", ^ (id, "[] = {\n")));
		resource_filewrite_i("0xff, 0xff, 0xff, 0xff,\n");
		for (i in /*to*/0... - (String.length(data), 1)) {
		var code = Char.code(String.unsafe_get(data, i));
			resource_filewrite(Printf.sprintf("%d,", code));
			if ( = (mod(i, 10), 9)) {
				resource_filewrite("\n");
			} else {
				[];
			};
		};
		resource_filewrite("0x00 };\n");
		incr(idx);
		resource_filewrite("}\n");
		resource_fileclose, common_ctx.resources);
		var resource_file = new_cpp_file(common_ctx, common_ctx.file, (new Tuple([], "__resources__")));
		resource_filewrite("#include <hxcpp.h>\n\n");
		resource_filewrite("namespace hx {\n");
		idx.val = 0;
		Hashtbl.iter(function _ : function data : var id = ^ ("__res_", string_of_int(idx.val));
					 resource_filewrite_i( ^ ("extern unsigned char ", ^ (id, "[];\n")));
					 incr(idx), common_ctx.resources);
		resource_filewrite("}\n\n");
		idx.val = 0;
		resource_filewrite("hx::Resource __Resources[] = ");
		resource_filebegin_block;
		Hashtbl.iter(function name : function data : var id = ^ ("__res_", string_of_int(idx.val));
					 resource_filewrite_i( ^ ("{ ", ^ (str(name), ^ (",", ^ (string_of_int(String.length(data)), ^ (",", ^ ("hx::", ^ (id, " + 4 },\n"))))))));
					 incr(idx), common_ctx.resources);
		resource_filewrite_i("{ ::String[null[]],0,0 }\n");
		resource_fileend_block_line;
		resource_filewrite(";\n\n");
		resource_filewrite("namespace hx { Resource *GetResources[] { return __Resources; } }\n");
		resource_fileclose;
	};

	public static function write_build_data(common_ctx, filename, classes, main_deps, boot_deps, build_extra, extern_src,
											exe_name) return {
		var buildfile = open_out(filename);
		var include_prefix = get_include_prefix(common_ctx, True);
		function add_class_to_buildfile(class_path, deps) return {
			var cpp = ^ (join_class_path(class_path, "/"), source_file_extension(common_ctx));
			output_string(buildfile, ^ ("  <file name=\"src/", ^ (cpp, "\">\n")));
			var project_deps = List.filter(function path: !(is_internal_class(path)), deps);
			List.iter(function path: output_string(buildfile, ^ ("   <depend name=\"", ^ (switch (path) {
		case (::(@verbatim, []), file): file;
			case _: ^ ("include/", ^ (include_prefix, ^ (join_class_path(path, "/"), ".h")));
			}, "\"/>\n"))), project_deps);
			output_string(buildfile, "  </file>\n");
		};
		function add_classdef_to_buildfile(Tuple(class_path, deps, _)) return {
			add_class_to_buildfile(class_path, deps);
		};
		output_string(buildfile, "<xml>\n");
		output_string(buildfile, ^ ("<set name=\"HXCPP_API_LEVEL\" value=\"", ^ (Common.defined_value(common_ctx, Define.HxcppApiLevel), "\" />\n")));
		output_string(buildfile, "<files id=\"haxe\">\n");
		output_string(buildfile, "<compilerflag value=\"-Iinclude\"/>\n");
		List.iter(add_classdef_to_buildfile, classes);
		add_class_to_buildfile((new Tuple([], "__boot__")), boot_deps);
		add_class_to_buildfile((new Tuple([], "__files__")), []);
		add_class_to_buildfile((new Tuple([], "__resources__")), []);
		output_string(buildfile, "</files>\n");
		output_string(buildfile, "<files id=\"__lib__\">\n");
		output_string(buildfile, "<compilerflag value=\"-Iinclude\"/>\n");
		add_class_to_buildfile((new Tuple([], "__lib__")), main_deps);
		output_string(buildfile, "</files>\n");
		output_string(buildfile, "<files id=\"__main__\">\n");
		output_string(buildfile, "<compilerflag value=\"-Iinclude\"/>\n");
		add_class_to_buildfile((new Tuple([], "__main__")), main_deps);
		output_string(buildfile, "</files>\n");
		output_string(buildfile, "<files id=\"__resources__\">\n");
		var idx = ref(0);
		Hashtbl.iter(function _: function data: var id = ^ ("__res_", string_of_int(idx.val));
					 output_string(buildfile, ^ ("<file name=\"src/resources/", ^ (id, ".cpp\" />\n")));
					 incr(idx), common_ctx.resources);
		output_string(buildfile, "</files>\n");
		output_string(buildfile, "<files id=\"__externs__\">\n");
		List.iter(function src: output_string(buildfile, ^ ("<file name=\"", ^ (src, "\" />\n"))), extern_src);
		output_string(buildfile, "</files>\n");
		output_string(buildfile, ^ ("<set name=\"HAXE_OUTPUT\" value=\"", ^ (exe_name, "\" />\n")));
		output_string(buildfile, "<include name=\"${HXCPP}/build-tool/BuildCommon.xml\"/>\n");
		output_string(buildfile, build_extra);
		output_string(buildfile, "</xml>\n");
		close_out(buildfile);
	};

	public static function write_build_options(common_ctx, filename, defines) return {
		var writer = cached_source_writer(common_ctx, filename);
		PMap.iter(function name: function value:
		switch (name) {
	case true | sys | dce | cpp | debug: [];
		case _: writerwrite( ^ (name, ^ ("=", ^ (escape_command(value), "\n"))));
		}, defines);
		var cmd = Unix.open_process_in("haxelib path hxcpp");
		writerwrite( ^ ("hxcpp=", Pervasives.input_line(cmd)));
		Pervasives.ignore(Unix.close_process_in(cmd));
		writerclose;
	};

	public static function create_member_types(common_ctx) return {
		var result = Hashtbl.create(0);
		function add_member(class_name, interface, member) return {
			switch ((new Tuple(follow(member.cf_type), member.cf_kind))) {
			case (_, Var(_)) if (interface): [];
			case (_, Method(MethDynamic)) if (interface): [];
			case (TFun(_, ret), _): Hashtbl.add(result, ^ (class_name, ^ (".", member.cf_name)), type_string(ret));
			case (_, _) if (!(interface)): Hashtbl.add(result, ^ (class_name, ^ (".", member.cf_name)), type_string(member.cf_type));
			case _: [];
			};
		};
		List.iter(function object_def:
		switch (object_def) {
	case TClassDecl(class_def): var class_name = ^ ("::", join_class_path(class_def.cl_path, "::"));
			function add_all_fields(class_def) return {
				if (class_def.cl_interface) {
					List.iter(function impl: add_all_fields(fst(impl)), class_def.cl_implements);
				} else {
					[];
				};
				switch (class_def.cl_super) {
				case Some(super): add_all_fields(fst(super));
				case _: [];
				};
				List.iter(add_member(class_name, class_def.cl_interface), class_def.cl_ordered_fields);
				List.iter(add_member(class_name, class_def.cl_interface), class_def.cl_ordered_statics);
			};
			add_all_fields(class_def);
		case _: [];
		}, common_ctx.types);
		result;
	};

	public static function create_super_dependencies(common_ctx) return {
		var result = Hashtbl.create(0);
		List.iter(function object_def:
		switch (object_def) {
	case TClassDecl(class_def) if (!(class_def.cl_extern)): var deps = ref([]);
			switch (class_def.cl_super) {
			case Some(super): if (!(fst(super).cl_extern)) {
					deps.val = ::(fst(super).cl_path, deps.val);
				} else {
					[];
				};
			case _: [];
			};
			List.iter(function imp:
			if (!(fst(imp).cl_extern)) {
			deps.val = ::(fst(imp).cl_path, deps.val);
			} else {
				[];
			}, real_interfaces(class_def.cl_implements));
			Hashtbl.add(result, class_def.cl_path, deps.val);
		case TEnumDecl(enum_def) if (!(enum_def.e_extern)): Hashtbl.add(result, enum_def.e_path, []);
		case _: [];
		}, common_ctx.types);
		result;
	};

	public static function create_constructor_dependencies(common_ctx) return {
		var result = Hashtbl.create(0);
		List.iter(function object_def:
		switch (object_def) {
	case TClassDecl(class_def) if (!(class_def.cl_extern)):
			switch (class_def.cl_constructor) {
			case Some(func_def): Hashtbl.add(result, class_def.cl_path, func_def);
			case _: [];
			};
		case _: [];
		}, common_ctx.types);
		result;
	};

	public static function is_this(expression) return {
		switch (remove_parens(expression).eexpr) {
		case TConst(TThis): True;
		case _: False;
		};
	};

	public static function is_super(expression) return {
		switch (remove_parens(expression).eexpr) {
		case TConst(TSuper): True;
		case _: False;
		};
	};

	public static function is_assign_op(op) return {
		switch (op) {
		case OpAssign | OpAssignOp(_): True;
		case _: False;
		};
	};

	public static function script_type_string(haxe_type) return {
		switch (haxe_type) {
		case TType({ t_path = ([], Null) }, ::(t, [])):
			switch (follow(t)) {
			case TAbstract({ a_path = ([], Int) }, _) | TAbstract({ a_path = ([], Float) }, _) | TAbstract({ a_path = ([], Bool) }, _) | TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = ([], Float) }, _) | TEnum({ e_path = ([], Bool) }, _)
					: "Dynamic";
			case _: script_type_string(t);
			};
		case TInst({ cl_path = ([], Null) }, ::(t, [])):
			switch (follow(t)) {
			case TAbstract({ a_path = ([], Int) }, _) | TAbstract({ a_path = ([], Float) }, _) | TAbstract({ a_path = ([], Bool) }, _) | TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = ([], Float) }, _) | TEnum({ e_path = ([], Bool) }, _)
					: "Dynamic";
			case _: script_type_string(t);
			};
		case _: switch (follow(haxe_type)) {
			case TType({ t_path = ([], Array) }, params): "Array";
			case TInst({ cl_path = ([], Array) }, params):
				switch (params) {
				case ::(t, []): switch (type_string_suff("", t, False)) {
					case int: "Array.int";
					case Float: "Array.Float";
					case bool: "Array.bool";
					case ::String: "Array.String";
					case unsigned char: "Array.unsigned char";
					case Dynamic: "Array.Any";
					case _: "Array.Object";
					};
				case _: "Array.Object";
				};
			case TAbstract(abs, pl) if (<>(abs.a_impl, None)): script_type_string(Abstract.get_underlying_type(abs, pl));
			case _: type_string_suff("", haxe_type, False);
			};
		};
	};

	public static function is_template_type(t) return {
		False;
	};

	public static function is_dynamic_in_cppia(ctx, expr) return {
		switch (expr.eexpr) {
		case TCast(_, None): True;
		case _: is_dynamic_in_cpp(ctx, expr);
		};
	};

	public static function cppia_op_info(match) return switch (match) {
	case IaFunction: (new Tuple("FUNCTION", 1));
	case IaVar: (new Tuple("VAR", 2));
	case IaToInterface: (new Tuple("TOINTERFACE", 3));
	case IaToDynArray: (new Tuple("TODYNARRAY", 4));
	case IaToDataArray: (new Tuple("TODATAARRAY", 5));
	case IaToInterfaceArray: (new Tuple("TOINTERFACEARRAY", 6));
	case IaFun: (new Tuple("FUN", 7));
	case IaCast: (new Tuple("CAST", 8));
	case IaBlock: (new Tuple("BLOCK", 9));
	case IaBreak: (new Tuple("BREAK", 10));
	case IaContinue: (new Tuple("CONTINUE", 11));
	case IaIsNull: (new Tuple("ISNULL", 12));
	case IaNotNull: (new Tuple("NOTNULL", 13));
	case IaSet: (new Tuple("SET", 14));
	case IaCall: (new Tuple("CALL", 15));
	case IaCallGlobal: (new Tuple("CALLGLOBAL", 16));
	case IaCallStatic: (new Tuple("CALLSTATIC", 17));
	case IaCallMember: (new Tuple("CALLMEMBER", 18));
	case IaCallSuper: (new Tuple("CALLSUPER", 19));
	case IaCallThis: (new Tuple("CALLTHIS", 20));
	case IaCallSuperNew: (new Tuple("CALLSUPERNEW", 21));
	case IaCreateEnum: (new Tuple("CREATEENUM", 22));
	case IaADef: (new Tuple("ADEF", 23));
	case IaIf: (new Tuple("IF", 24));
	case IaIfElse: (new Tuple("IFELSE", 25));
	case IaFName: (new Tuple("FNAME", 27));
	case IaFStatic: (new Tuple("FSTATIC", 28));
	case IaFThisInst: (new Tuple("FTHISINST", 29));
	case IaFLink: (new Tuple("FLINK", 30));
	case IaFThisName: (new Tuple("FTHISNAME", 31));
	case IaFEnum: (new Tuple("FENUM", 32));
	case IaThrow: (new Tuple("THROW", 33));
	case IaArrayI: (new Tuple("ARRAYI", 34));
	case IaPlusPlus: (new Tuple("++", 35));
	case IaPlusPlusPost: (new Tuple("+++", 36));
	case IaMinusMinus: (new Tuple("--", 37));
	case IaMinusMinusPost: (new Tuple("---", 38));
	case IaNeg: (new Tuple("NEG", 39));
	case IaBitNot: (new Tuple("~", 40));
	case IaLogicNot: (new Tuple("!", 41));
	case IaTVars: (new Tuple("TVARS", 42));
	case IaVarDecl: (new Tuple("VARDECL", 43));
	case IaVarDeclI: (new Tuple("VARDECLI", 44));
	case IaNew: (new Tuple("NEW", 45));
	case IaReturn: (new Tuple("RETURN", 46));
	case IaRetVal: (new Tuple("RETVAL", 47));
	case IaPosInfo: (new Tuple("POSINFO", 48));
	case IaObjDef: (new Tuple("OBJDEF", 49));
	case IaClassOf: (new Tuple("CLASSOF", 50));
	case IaWhile: (new Tuple("WHILE", 51));
	case IaFor: (new Tuple("FOR", 52));
	case IaEnumI: (new Tuple("ENUMI", 53));
	case IaSwitch: (new Tuple("SWITCH", 54));
	case IaTry: (new Tuple("TRY", 55));
	case IaImplDynamic: (new Tuple("IMPLDYNAMIC", 56));
	case IaConstInt: (new Tuple("i", 57));
	case IaConstFloat: (new Tuple("f", 58));
	case IaConstString: (new Tuple("s", 59));
	case IaConstFalse: (new Tuple("false", 60));
	case IaConstTrue: (new Tuple("true", 61));
	case IaConstNull: (new Tuple("NULL", 62));
	case IaConsThis: (new Tuple("THIS", 63));
	case IaConstSuper: (new Tuple("SUPER", 64));
	case IaCastInt: (new Tuple("CASTINT", 65));
	case IaCastBool: (new Tuple("CASTBOOL", 66));
	case IaInterface: (new Tuple("INTERFACE", 67));
	case IaClass: (new Tuple("CLASS", 68));
	case IaAccessNormal: (new Tuple("N", 69));
	case IaAccessNot: (new Tuple("n", 70));
	case IaAccessResolve: (new Tuple("R", 71));
	case IaAccessCall: (new Tuple("C", 72));
	case IaEnum: (new Tuple("ENUM", 73));
	case IaInline: (new Tuple("INLINE", 74));
	case IaMain: (new Tuple("MAIN", 75));
	case IaNoMain: (new Tuple("NOMAIN", 76));
	case IaResources: (new Tuple("RESOURCES", 77));
	case IaReso: (new Tuple("RESO", 78));
	case IaNoCast: (new Tuple("NOCAST", 79));
	case IaAccessCallNative: (new Tuple("V", 80));
	case IaBinOp(OpAdd): (new Tuple("+", 101));
	case IaBinOp(OpMult): (new Tuple("*", 102));
	case IaBinOp(OpDiv): (new Tuple("/", 103));
	case IaBinOp(OpSub): (new Tuple("-", 104));
	case IaBinOp(OpAssign): (new Tuple("=", 105));
	case IaBinOp(OpEq): (new Tuple("==", 106));
	case IaBinOp(OpNotEq): (new Tuple("!=", 107));
	case IaBinOp(OpGte): (new Tuple(">=", 108));
	case IaBinOp(OpLte): (new Tuple("<=", 109));
	case IaBinOp(OpGt): (new Tuple(">", 110));
	case IaBinOp(OpLt): (new Tuple("<", 111));
	case IaBinOp(OpAnd): (new Tuple("&", 112));
	case IaBinOp(OpOr): (new Tuple("|", 113));
	case IaBinOp(OpXor): (new Tuple("^", 114));
	case IaBinOp(OpBoolAnd): (new Tuple("&&", 115));
	case IaBinOp(OpBoolOr): (new Tuple("||", 116));
	case IaBinOp(OpShr): (new Tuple("> >", 117));
	case IaBinOp(OpUShr): (new Tuple("> > >", 118));
	case IaBinOp(OpShl): (new Tuple("< <", 119));
	case IaBinOp(OpMod): (new Tuple("%", 120));
	case IaBinOp(OpInterval): (new Tuple("...", 121));
	case IaBinOp(OpArrow): (new Tuple("=>", 122));
	case IaBinOp(OpAssignOp(OpAdd)): (new Tuple("+=", 201));
	case IaBinOp(OpAssignOp(OpMult)): (new Tuple("*=", 202));
	case IaBinOp(OpAssignOp(OpDiv)): (new Tuple("/=", 203));
	case IaBinOp(OpAssignOp(OpSub)): (new Tuple("-=", 204));
	case IaBinOp(OpAssignOp(OpAnd)): (new Tuple("&=", 212));
	case IaBinOp(OpAssignOp(OpOr)): (new Tuple("|=", 213));
	case IaBinOp(OpAssignOp(OpXor)): (new Tuple("^=", 214));
	case IaBinOp(OpAssignOp(OpBoolAnd)): (new Tuple("&&=", 215));
	case IaBinOp(OpAssignOp(OpBoolOr)): (new Tuple("||=", 216));
	case IaBinOp(OpAssignOp(OpShr)): (new Tuple("> >=", 217));
	case IaBinOp(OpAssignOp(OpUShr)): (new Tuple("> > >=", 218));
	case IaBinOp(OpAssignOp(OpShl)): (new Tuple("< <=", 219));
	case IaBinOp(OpAssignOp(OpMod)): (new Tuple("%=", 220));
	case IaBinOp(OpAssignOp(OpInterval)) | IaBinOp(OpAssignOp(OpAssign)) | IaBinOp(OpAssignOp(OpEq)) | IaBinOp(OpAssignOp(OpNotEq)) | IaBinOp(OpAssignOp(OpGte)) | IaBinOp(OpAssignOp(OpLte)) | IaBinOp(OpAssignOp(OpGt)) | IaBinOp(OpAssignOp(OpLt)) | IaBinOp(OpAssignOp(OpAssignOp(_))) | IaBinOp(OpAssignOp(OpArrow))
			: assert False;
	};

	public static function generate_script_class(common_ctx, script, class_def) return {
		scriptincClasses;
		scriptwriteOp(if (class_def.cl_interface) {
		IaInterface;
	} else {
		IaClass;
	});
		scriptinstName(class_def);
		switch (class_def.cl_super) {
		case None: scriptident("");
		case Some(c, _): scriptinstName(c);
		};
		scriptwint(List.length(class_def.cl_implements));
		List.iter(function (c, _): scriptinstName(c), class_def.cl_implements);
		scriptwrite("\n");
		function non_dodgy_function(field) return {
			|| (class_def.cl_interface, switch ((new Tuple(field.cf_kind, field.cf_expr))) {
		case (Var(_), _): True;
			case (Method(MethDynamic), _): True;
			case (Method(_), Some(_)): True;
			case _: False;
			});
		};
		var ordered_statics = List.filter(non_dodgy_function, class_def.cl_ordered_statics);
		var ordered_fields = List.filter(non_dodgy_function, class_def.cl_ordered_fields);
		scriptwrite( ^ (string_of_int(+(+(+(+(List.length(ordered_fields), List.length(ordered_statics)), switch (class_def.cl_constructor) {
	case Some(_): 1;
		case _: 0;
		}), if (implement_dynamic_here(class_def)) {
		1;
	} else {
		0;
	}), switch (class_def.cl_init) {
	case Some(_): 1;
		case _: 0;
		})), "\n"));
		function generate_field(isStatic, field) return {
			switch ((new Tuple(field.cf_kind, follow(field.cf_type)))) {
			case (Var({ v_read = AccInline; v_write = AccNever }), _): scriptwriteOpLine(IaInline);
			case (Var(v), _): 	function mode_code(mode) return {
					switch (mode) {
					case AccNormal: IaAccessNormal;
					case AccNo: IaAccessNot;
					case AccNever: IaAccessNot;
					case AccResolve: IaAccessResolve;
					case AccCall: if ( || (has_meta_key(class_def.cl_meta, Meta.NativeProperty),
						|| (has_meta_key(field.cf_meta, Meta.NativeProperty), Common.defined(common_ctx, Define.ForceNativeProperty)))) {
							IaAccessCallNative;
						} else {
							IaAccessCall;
						};
					case AccInline: IaAccessNormal;
					case AccRequire(_, _): IaAccessNormal;
					};
				};
				var isExtern = is_extern_field(field);
				scriptvar(mode_code(v.v_read), mode_code(v.v_write), isExtern, isStatic, field.cf_name, field.cf_type, field.cf_expr);
			case (Method(MethDynamic), TFun(args, ret)): scriptfunc(isStatic, True, field.cf_name, ret, args, class_def.cl_interface,
						field.cf_expr);
			case (Method(_), TFun(args, ret)) if (=(field.cf_name, "new")): scriptfunc(True, False, "new", TInst(class_def, []), args,
						False, field.cf_expr);
			case (Method(_), TFun(args, ret)): scriptfunc(isStatic, False, field.cf_name, ret, args, class_def.cl_interface,
						field.cf_expr);
			case (Method(_), _): print_endline( ^ ("Unknown method type ", ^ (join_class_path(class_def.cl_path, "."), ^ (".",
													   field.cf_name))));
			};
		};
		switch (class_def.cl_constructor) {
		case Some(field): generate_field(True, field);
		case _: [];
		};
		switch (class_def.cl_init) {
		case Some(expression): scriptvoidFunc(True, False, "__init__", expression);
		case _: [];
		};
		List.iter(generate_field(False), ordered_fields);
		List.iter(generate_field(True), ordered_statics);
		if (implement_dynamic_here(class_def)) {
			scriptimplDynamic;
		} else {
			[];
		};
		scriptwrite("\n");
	};

	public static function generate_script_enum(common_ctx, script, enum_def, meta) return {
		scriptincClasses;
		var sorted_items = List.sort(function f1: function f2: -(f1.ef_index, f2.ef_index), pmap_values(enum_def.e_constrs));
		scriptwriteList( ^ (scriptop(IaEnum), scriptenumText(enum_def)), List.length(sorted_items));
		List.iter(function constructor: var name = scriptstringText(constructor.ef_name);
		switch (constructor.ef_type) {
	case TFun(args, _): scriptwrite( ^ (name, ^ (" ", string_of_int(List.length(args)))));
			List.iter(function (arg, _, t): scriptwrite( ^ (" ", ^ (scriptstringText(arg), ^ (" ", scripttypeText(t))))), args);
			scriptwrite("\n");
		case _: scriptwrite( ^ (name, " 0\n"));
		}, sorted_items);
		switch (meta) {
		case Some(expr): scriptwrite("1\n");
			scriptgen_expression(expr);
		case _: scriptwrite("0\n");
			scriptwrite("\n");
		};
	};

	public static function generate_cppia(common_ctx) return {
		var debug = 1;
		var null_file = new source_writer(common_ctx, ignore, function []: []);
		var ctx = new_context(common_ctx, null_file, debug, ref(PMap.empty));
		ctx.ctx_class_member_types = create_member_types(common_ctx);
		var script = new script_writer(common_ctx, ctx, common_ctx.file, common_ctx.debug);
		ignore(scriptstringId(""));
		ignore(scripttypeId(""));
		List.iter(function object_def:
		switch (object_def) {
	case TClassDecl(class_def) if (class_def.cl_extern): [];
		case TClassDecl(class_def): var is_internal = is_internal_class(class_def.cl_path);
			if ( || (is_internal, is_macro(class_def.cl_meta))) {
				if ( > (debug, 1)) {
					print_endline( ^ (" internal class ", join_class_path(class_def.cl_path, ".")));
				} else {
					[];
				};
			} else {
				ctx.ctx_class_name = ^ ("::", join_class_path(class_def.cl_path, "::"));
				generate_script_class(common_ctx, script, class_def);
			};
		case TEnumDecl(enum_def) if (enum_def.e_extern): [];
		case TEnumDecl(enum_def): var is_internal = is_internal_class(enum_def.e_path);
			if (is_internal) {
				if ( > (debug, 1)) {
					print_endline( ^ (" internal enum ", join_class_path(enum_def.e_path, ".")));
				} else {
					[];
				};
			} else {
				var meta = Codegen.build_metadata(common_ctx, object_def);
				if (enum_def.e_extern) {
					if ( > (debug, 1)) {
						print_endline( ^ ("external enum ", join_class_path(enum_def.e_path, ".")));
					} else {
						[];
					};
				} else {
					[];
				};
				ctx.ctx_class_name = "*";
				generate_script_enum(common_ctx, script, enum_def, meta);
			};
		case TTypeDecl(_) | TAbstractDecl(_): [];
		}, common_ctx.types);
		switch (common_ctx.main) {
		case None: scriptwriteOpLine(IaNoMain);
		case Some(e): scriptwriteOpLine(IaMain);
			scriptgen_expression(e);
		};
		scriptwrite( ^ (scriptop(IaResources), ^ (string_of_int(Hashtbl.length(common_ctx.resources)), "\n")));
		Hashtbl.iter(function name: function data: scriptwrite( ^ (scriptop(IaReso), ^ (scriptstringText(name), ^ (string_of_int(String.length(data)), "\n")))), common_ctx.resources);
		Hashtbl.iter(function _: function data: scriptwriteData(data), common_ctx.resources);
		scriptclose;
	};

	public static function generate_source(common_ctx) return {
		make_base_directory(common_ctx.file);
		var debug = 1;
		var exe_classes = ref([]);
		var boot_classes = ref([]);
		var boot_enums = ref([]);
		var nonboot_classes = ref([]);
		var init_classes = ref([]);
		var file_info = ref(PMap.empty);
		function class_text(path) return {
			join_class_path(path, "::");
		};
		var member_types = create_member_types(common_ctx);
		var super_deps = create_super_dependencies(common_ctx);
		var constructor_deps = create_constructor_dependencies(common_ctx);
		var main_deps = ref([]);
		var extern_src = ref([]);
		var build_xml = ref("");
		var scriptable = Common.defined(common_ctx, Define.Scriptable);
		List.iter(function object_def:
		if (!(Common.defined(common_ctx, Define.Objc))) {
		switch (object_def) {
			case TClassDecl(class_def) if (Meta.has(Meta.Objc, class_def.cl_meta)):
				error("In order to compile '@:objc' classes, please define '-D objc'", class_def.cl_pos);
			case _: [];
			};
		} else {
			[];
		};
		switch (object_def) {
	case TClassDecl(class_def) if (is_extern_class(class_def)): build_xml.val = ^ (build_xml.val, get_class_code(class_def,
					Meta.BuildXml));
			var source = get_meta_string_path(class_def.cl_meta, Meta.SourceFile);
			if (<>(source, "")) {
				extern_src.val = ::(source, extern_src.val);
			} else {
				[];
			};
		case TClassDecl(class_def): var name = class_text(class_def.cl_path);
			var is_internal = is_internal_class(class_def.cl_path);
			if ( || (is_internal, is_macro(class_def.cl_meta))) {
				if ( > (debug, 1)) {
					print_endline( ^ (" internal class ", name));
				} else {
					[];
				};
			} else {
				build_xml.val = ^ (build_xml.val, get_class_code(class_def, Meta.BuildXml));
				if (has_init_field(class_def)) {
					init_classes.val = ::(class_def.cl_path, init_classes.val);
				} else {
					[];
				};
				if (has_boot_field(class_def)) {
					boot_classes.val = ::(class_def.cl_path, boot_classes.val);
				} else {
					if (!(has_meta_key(class_def.cl_meta, Meta.NativeGen))) {
						nonboot_classes.val = ::(class_def.cl_path, nonboot_classes.val);
					} else {
						[];
					};
				};
				var deps = generate_class_files(common_ctx, member_types, super_deps, constructor_deps, class_def, file_info, scriptable);
				exe_classes.val = ::((new Tuple(class_def.cl_path, deps, object_def)), exe_classes.val);
			};
		case TEnumDecl(enum_def) if (enum_def.e_extern): [];
		case TEnumDecl(enum_def): var name = class_text(enum_def.e_path);
			var is_internal = is_internal_class(enum_def.e_path);
			if (is_internal) {
				if ( > (debug, 1)) {
					print_endline( ^ (" internal enum ", name));
				} else {
					[];
				};
			} else {
				var meta = Codegen.build_metadata(common_ctx, object_def);
				if (enum_def.e_extern) {
					if ( > (debug, 1)) {
						print_endline( ^ ("external enum ", name));
					} else {
						[];
					};
				} else {
					[];
				};
				boot_enums.val = ::(enum_def.e_path, boot_enums.val);
				var deps = generate_enum_files(common_ctx, enum_def, super_deps, meta, file_info);
				exe_classes.val = ::((new Tuple(enum_def.e_path, deps, object_def)), exe_classes.val);
			};
		case TTypeDecl(_) | TAbstractDecl(_): [];
		}, common_ctx.types);
		switch (common_ctx.main) {
		case None: generate_dummy_main(common_ctx);
		case Some(e): var main_field = { () with cf_name = "__main__";
											 cf_type = t_dynamic;
											 cf_expr = Some(e);
											 cf_pos = e.epos;
											 cf_public = True;
											 cf_meta = [];
											 cf_overloads = [];
											 cf_doc = None;
											 cf_kind = Var({ () with v_read = AccNormal;
													 v_write = AccNormal
														   });
											 cf_params = []
										   };
			var class_def = { (null_class) with cl_path = (new Tuple([], "@Main"));
							  cl_ordered_statics = ::(main_field, [])
							};
			main_deps.val = find_referenced_types(common_ctx, TClassDecl(class_def), super_deps, constructor_deps, False, True, False);
			generate_main(common_ctx, member_types, super_deps, class_def, file_info);
		};
		generate_boot(common_ctx, boot_enums.val, boot_classes.val, nonboot_classes.val, init_classes.val);
		generate_files(common_ctx, file_info);
		write_resources(common_ctx);
		if ( || (scriptable, Common.defined(common_ctx, Define.DllExport))) {
			var filename = try {
				var value = Common.defined_value(common_ctx, Define.DllExport);
				if ( = (value, "1")) {
					raise(Not_found);
				} else {
					[];
				};
				value;
			} catch (e: Not_found) {
				"export_classes.info";
			};
			if (<>(filename, "")) {
				function escape(s) return {
					var b = Buffer.create(0);
					for (i in /*to*/0... - (String.length(s), 1)) {
						var c = String.unsafe_get(s, i);
						switch (c) {
						case '\\': Buffer.add_char(b, c);
							Buffer.add_char(b, c);
						case ' ': Buffer.add_char(b, '\\');
							Buffer.add_char(b, 's');
						case '\n': Buffer.add_char(b, '\\');
							Buffer.add_char(b, 'n');
						case _: Buffer.add_char(b, c);
						};
					};
					Buffer.contents(b);
				};
				var exeClasses = open_out(filename);
				var out = output_string(exeClasses);
				function outline(str) return {
					output_string(exeClasses, ^ (str, "\n"));
				};
				function spath(path) return {
					join_class_path(path, ".");
				};
				function stype(match) return switch (match) {
				case TMono(r): switch (r.val) {
					case None: "Dynamic";
					case Some(t): stype(t);
					};
				case TAbstract({ a_path = ([], Void) }, []): "void";
				case TAbstract({ a_path = ([], Bool) }, []): "bool";
				case TAbstract({ a_path = ([], Float) }, []): "float";
				case TAbstract({ a_path = ([], Int) }, []): "int";
				case TAbstract({ a_path = ([], EnumValue) }, _): "Dynamic";
				case TEnum(enum, params): spath(enum.e_path);
				case TInst(klass, params): switch ((new Tuple(klass.cl_path, params))) {
					case (_, _) if (is_dynamic_type_param(klass.cl_kind)): "Dynamic";
					case (([], Array), ::(t, [])): ^ ("Array<", ^ (stype(t), ">"));
					case ((::(haxe, ::(io, [])), Unsigned_char__), _): "uint8";
					case (([], EnumValue), _): "Dynamic";
					case (([], Null), ::(t, [])) if (cant_be_null(t)): ^ ("Null<", ^ (stype(t), ">"));
					case (([], Null), ::(t, [])): stype(t);
					case _: spath(klass.cl_path);
					};
				case TType(type_def, params): switch ((new Tuple(type_def.t_path, params))) {
					case (([], Null), ::(t, [])) if (cant_be_null(t)): ^ ("Null<", ^ (stype(t), ">"));
					case (([], Array), ::(t, [])): ^ ("Array< ", ^ (stype(follow(t)), " >"));
					case (_, _): stype(apply_params(type_def.t_params, params, type_def.t_type));
					};
				case TLazy(func): stype(func.val([]));
				case TAbstract(abs, pl) if (<>(abs.a_impl, None)): stype(Abstract.get_underlying_type(abs, pl));
				case TAbstract(abs, _): spath(abs.a_path);
				case TFun(args, ret): ^ ("fun<", ^ (List.fold_left(function s: function (_, opt, t): ^ (s, ^ (if (opt) {
					"?";
				} else {
					"";
				}, ^ (stype(t), ","))), "", args), ^ (stype(ret), ">")));
				case _: "Dynamic";
				};
				List.iter(function (name, _, def):
				switch (def) {
			case TClassDecl(class_def): outline( ^ (if (class_def.cl_interface) {
					"interface ";
				} else {
					"class ";
				}, spath(name)));
					switch (class_def.cl_super) {
					case Some(super, _): outline( ^ ("super ", spath(super.cl_path)));
					case _: [];
					};
					List.iter(function (c, _): out( ^ ("implements ", ^ (spath(c.cl_path), "\n"))), class_def.cl_implements);
					switch (class_def.cl_dynamic) {
					case None: [];
					case Some(t): outline( ^ ("implementsdynamic ", stype(t)));
					};
					switch (class_def.cl_array_access) {
					case None: [];
					case Some(t): outline( ^ ("arrayaccess ", stype(t)));
					};
					function args(match) return switch (match) {
					case TFun(args, _): List.iter(function (name, opt, t): outline( ^ ("arg ", ^ (name, ^ (if (opt) {
						" ? ";
					} else {
						" : ";
					}, stype(t))))), args);
					case _: [];
					};
					function ret(match) return switch (match) {
					case TFun(_, ret): stype(ret);
					case _: "Dynamic";
					};
					function print_field(stat, f) return {
						var pub = if (f.cf_public) {
							"pub ";
						} else {
							"priv ";
						};
						var stat = ^ (pub, if (stat) {
						"s ";
					} else {
						"m ";
					});
						switch ((new Tuple(f.cf_kind, f.cf_name))) {
						case (Var({ v_read = AccInline; v_write = AccNever }), _): outline( ^ ("inlinevar ", ^ (f.cf_name, ^ (" ",
							stype(f.cf_type)))));
						case (Var({ v_read = AccNormal; v_write = AccNormal }), _): outline( ^ ("var ", ^ (stat, ^ (f.cf_name, ^ (" ",
									stype(f.cf_type))))));
						case (Var(v), _): function saccess(match) return switch (match) {
							case AccNormal: "v";
							case AccNo: "0";
							case AccNever: "!";
							case AccResolve: "r";
							case AccCall: "c";
							case AccInline: "i";
							case AccRequire(_, _): "v";
							};
							outline( ^ ("property ", ^ (stat, ^ (saccess(v.v_read), ^ (" ", ^ (saccess(v.v_write), ^ (" ", ^ (f.cf_name, ^ (" ",
																 stype(f.cf_type))))))))));
						case (Method(_), new): outline( ^ ("function ", ^ (stat, ^ ("new ", ret(f.cf_type)))));
							args(f.cf_type);
						case (Method(MethDynamic), _): outline( ^ ("dynamicfunction ", ^ (stat, ^ (f.cf_name, ^ (" ", ret(f.cf_type))))));
							args(f.cf_type);
						case (Method(_), _): outline( ^ ("function ", ^ (stat, ^ (f.cf_name, ^ (" ", ret(f.cf_type))))));
							args(f.cf_type);
						};
					};
					switch (class_def.cl_constructor) {
					case None: [];
					case Some(f): print_field(False, f);
					};
					List.iter(print_field(False), class_def.cl_ordered_fields);
					List.iter(print_field(True), class_def.cl_ordered_statics);
				case TEnumDecl(enum_def): out( ^ ("enum ", ^ (spath(name), "\n")));
					var sorted_items = List.sort(function f1: function f2: -(f1.ef_index, f2.ef_index), pmap_values(enum_def.e_constrs));
					List.iter(function constructor: outline( ^ ("constructor ", constructor.ef_name));
					switch (constructor.ef_type) {
				case TFun(args, _): List.iter(function (arg, _, t): outline( ^ ("eparam ", ^ (arg, ^ (" ", stype(t))))), args);
					case _: [];
					}, sorted_items);
				case _: [];
				}, exe_classes.val);
				List.iter(function file: var full_path = Common.get_full_path(try {
					Common.find_file(common_ctx, file);
				} catch (e: Not_found) {
					file;
				});
				out( ^ ("file ", ^ (escape(file), ^ (" ", ^ (escape(full_path), "\n"))))), List.sort(String.compare,
						pmap_keys(file_info.val)));
				close_out(exeClasses);
			} else {
				[];
			};
		} else {
			[];
		};
		var output_name = switch (common_ctx.main_class) {
		case Some(path): snd(path);
		case _: "output";
		};
		write_build_data(common_ctx, ^ (common_ctx.file, "/Build.xml"), exe_classes.val, main_deps.val, @(boot_enums.val, boot_classes.val), build_xml.val, extern_src.val, output_name);
		var cmd_defines = ref("");
		PMap.iter(function name: function value:
		switch (name) {
	case true | sys | dce | cpp | debug: [];
		case _: cmd_defines.val = ^ (cmd_defines.val, ^ (" -D", ^ (name, ^ ("=\"", ^ (escape_command(value), "\"")))));
		}, common_ctx.defines);
		write_build_options(common_ctx, ^ (common_ctx.file, "/Options.txt"), common_ctx.defines);
		if (!(Common.defined(common_ctx, Define.NoCompilation))) {
			var t = Common.timer("generate cpp - native compilation");
			var old_dir = Sys.getcwd([]);
			Sys.chdir(common_ctx.file);
			var cmd = ref("haxelib run hxcpp Build.xml haxe");
			if (common_ctx.debug) {
				cmd.val = ^ (cmd.val, " -Ddebug");
			} else {
				[];
			};
			cmd.val = ^ (cmd.val, cmd_defines.val);
			cmd.val = List.fold_left(function cmd: function path: ^ (cmd, ^ (" -I\"", ^ (escape_command(path), "\""))), cmd.val,
									 common_ctx.class_path);
			print_endline(cmd.val);
			if (<>(common_ctx.run_command(cmd.val), 0)) {
				failwith("Build failed");
			} else {
				[];
			};
			Sys.chdir(old_dir);
			t([]);
		} else {
			[];
		};
	};

	public static function generate(common_ctx) return {
		if (Common.defined(common_ctx, Define.Cppia)) {
			generate_cppia(common_ctx);
		} else {
			generate_source(common_ctx);
		};
	}
}
;
