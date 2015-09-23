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
enum Small_type {
	TNull;
	TBool(value: Bool);
	TFloat(value: Float);
	TString(value: String);
};
