unit Constraints;

interface

uses
	Classes,
	SysUtils;

type
	TConstraints = class
	public
		// Pointer, object, interface, or GUID cannot be nil (or the NULL guid)
		class procedure NotNull(const o: TObject; msg: string=''); overload;
		class procedure NotNull(const Value: IUnknown; msg: string=''); overload;
		class procedure NotNull(const Value: Pointer; msg: string=''); overload;
		class procedure NotNull(const Value: TGUID; msg: string=''); overload;

		class procedure NotNullIntf(const Value: IUnknown; msg: string=''); overload; // not needed; you can use NotNull(unk). Only exists to provide parity with Delphi 5 where you can't pass an IUnknown overload

		// String, list, or array must have a length greater than zero
		class procedure NotEmpty(const Value: string; msg: string=''); overload;
		class procedure NotEmpty(const Value: TList; msg: string=''); overload;

		// String must not be blank, i.e. cannot contain just whitespace
		class procedure NotBlank(const Value: string; msg: string='');

		class procedure Same(const Expected, Actual: Int64; msg: string='');

		// Specifies the minimum value of a numeric variable
		class procedure Min(const MinValue, Actual: Int64; msg: string='');

		class procedure Positive(const Value: Real; msg: string='');

		class procedure NonZero(const Value: Real; msg: string='');

		class procedure AssertFalse(const Value: Boolean; msg: string='');
		class procedure AssertTrue(const Value: Boolean; msg: string='');

{
		@NotNull				The annotated element must not be null
		@NotEmpty			The annotated element must not be empty, i.e. a string, list, or array must contain at least one element
		@NotBlank			The annotated element must not be blank, i.e. cannot contain just whitespace
		@Null					The annotated element must be null

		@Size					The annotated element must have a size within the specified range

		@Min					The annotated element must be a number whose value must be greater than or equal to the specified minimum
		@Max					The annotated element must be a number whose value must be less than or equal to the specified maximum
		@Positive			The annotated element must be a number whose value must be positive
		@PositiveOrZero	The annotated element must be a number whose value must be positive or zero
		@Negative			The annotated element must be a number whose value must be negative
		@NegativeOrZero	The annotated element must be a number whose value must be negative or zero

		@DecimalMax			The annotated element must be a number whose value must be less than or equal to the specified maximum
		@DecimalMin			The annotated element must be a number whose value must be greater than or equal to the specified minimum
		@Digits				The annotated element must be a number within accepted range of digits

		@Future				The annotated element must be a date in the future
		@Past					The annotated element must be a date in the past

		@AssertFalse		The annotated element must be false
		@AssertTrue			The annotated element must be true
		@Email				The annotated element must be a valid email address
		@Pattern				The annotated element must match the specified regular expression
}
	end;

type
	EConstraintViolation = class(EAssertionFailed);


implementation

{ TConstraints }

class procedure TConstraints.NotNull(const o: TObject; msg: string);
begin
	if o = nil then
		raise EConstraintViolation.CreateFmt('Object cannot be nil %s', [msg]);
end;

class procedure TConstraints.NotNull(const Value: IInterface; msg: string);
begin
	if value = nil then
		raise EConstraintViolation.CreateFmt('Interface cannot be nil %s', [msg]);
end;

class procedure TConstraints.NotNull(const Value: Pointer; msg: string);
begin
	if value = nil then
		raise EConstraintViolation.CreateFmt('Pointer cannot be nil %s', [msg]);
end;

class procedure TConstraints.NotEmpty(const Value: string; msg: string);
begin
	if Value = '' then
		raise EConstraintViolation.CreateFmt('String cannot be empty %s', [msg]);
end;

class procedure TConstraints.NonZero(const Value: Real; msg: string='');
begin
	if Value = 0 then
		raise EConstraintViolation.CreateFmt('Real value cannot be zero %s', [msg]);
end;

class procedure TConstraints.NotBlank(const Value: string; msg: string);
var
	s: string;
begin
	s := Trim(Value);

	if s = '' then
		raise EConstraintViolation.CreateFmt('String cannot be blank %s', [msg]);
end;

class procedure TConstraints.NotEmpty(const Value: TList; msg: string);
begin
	TConstraints.NotNull(Value, msg);

	if Value.Count <= 0 then
		raise EConstraintViolation.CreateFmt('%s cannot be empty %s', [Value.ClassName, msg]);
end;

class procedure TConstraints.NotNull(const Value: TGUID; msg: string);
begin
	if Value = TGUID.Empty then
		raise EConstraintViolation.CreateFmt('GUID cannot be empty %s', [msg]);
end;

class procedure TConstraints.NotNullIntf(const Value: IUnknown; msg: string);
begin
{
	This method isn't really needed in XE6.

	It's only provided so that you can have shared code between Delphi XE6 and Delphi 5.

	Delphi 5 doesn't know how to pass an interfaced variable to an overload that takes:

		- NotNull(TObject)
		- NotNull(IUnknown)

	So Delphi 5 created:

		- NotNullIntf(IUnknown)

	So you don't have to worry about calling NotNullIntf, and in fact i would argue that you shouldn't,
	unless you want code parity between Delphi 5 and XE6.
}
	if value = nil then
		raise EConstraintViolation.CreateFmt('Interface cannot be nil %s', [msg]);
end;

class procedure TConstraints.Positive(const Value: Real; msg: string='');
begin
	if Value <= 0 then
		raise EConstraintViolation.CreateFmt('Expected %f to be positive %s', [Value, msg]);
end;

class procedure TConstraints.Same(const Expected, Actual: Int64; msg: string);
begin
	if Expected <> Actual then
		raise EConstraintViolation.CreateFmt('Expected %d but was %d %s', [Expected, Actual, msg]);
end;

class procedure TConstraints.AssertFalse(const Value: Boolean; msg: string);
const
	SBoolean: array[Boolean] of string = ('False', 'True');
begin
	if Value <> False then
		raise EConstraintViolation.CreateFmt('Value %s must be False %s', [SBoolean[Value], msg]);
end;

class procedure TConstraints.AssertTrue(const Value: Boolean; msg: string);
const
	SBoolean: array[Boolean] of string = ('False', 'True');
begin
	if Value <> True then
		raise EConstraintViolation.CreateFmt('Value %s must be True %s', [SBoolean[Value], msg]);
end;

class procedure TConstraints.Min(const MinValue, Actual: Int64; msg: string);
begin
	if Actual < MinValue then
		raise EConstraintViolation.CreateFmt('Value %d must be >= than %d %s', [Actual, MinValue, msg]);
end;



end.
