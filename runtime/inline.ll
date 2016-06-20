define private %clsvar* @new_address_table() nounwind alwaysinline
{
  ret %clsvar* null
}

declare %mzone* @llvm_peek_zone_stack_extern() nounwind
define private %mzone* @llvm_peek_zone_stack() nounwind alwaysinline "thunk"
{
  %zone = call %mzone* @llvm_peek_zone_stack_extern()
  ret %mzone* %zone
}

declare void @llvm_push_zone_stack_extern(%mzone*) nounwind
define private void @llvm_push_zone_stack(%mzone* %zone) nounwind alwaysinline "thunk"
{
  call void @llvm_push_zone_stack_extern(%mzone* %zone)
  ret void
}

declare %mzone* @llvm_zone_create_extern(i64) nounwind
define private %mzone* @llvm_zone_create(i64 %size) nounwind alwaysinline "thunk"
{
  %zone = call %mzone* @llvm_zone_create_extern(i64 %size)
  ret %mzone* %zone
}

define private void @llvm_zone_mark(%mzone* %zone) nounwind alwaysinline
{
  %offset_ptr = getelementptr inbounds %mzone, %mzone* %zone, i32 0, i32 1
  %offset_val = load i64, i64* %offset_ptr
  %mark_ptr = getelementptr %mzone, %mzone* %zone, i32 0, i32 2
  store i64 %offset_val, i64* %mark_ptr
  ret void
}

define private i64 @llvm_zone_mark_size(%mzone* %zone) nounwind alwaysinline
{
  %offset_ptr = getelementptr inbounds %mzone, %mzone* %zone, i32 0, i32 1
  %offset_val = load i64, i64* %offset_ptr
  %mark_ptr = getelementptr %mzone, %mzone* %zone, i32 0, i32 2
  %mark_val = load i64, i64* %mark_ptr
  %res = sub i64 %offset_val, %mark_val
  ret i64 %res
}

define private %mzone* @llvm_zone_reset(%mzone* %zone) nounwind alwaysinline
{
  %offset_ptr = getelementptr inbounds %mzone, %mzone* %zone, i32 0, i32 1
  store i64 0, i64* %offset_ptr
  ret %mzone* %zone
}

declare i32 @is_integer_extern(i8*)
define private i32 @is_integer(i8* %ptr) nounwind alwaysinline
{
  %res = call i32 @is_integer_extern(i8* %ptr)
  ret i32 %res
}
