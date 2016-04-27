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

declare %mzone* @llvm_zone_reset_extern(%mzone*) nounwind
define private %mzone* @llvm_zone_reset(%mzone* %zone) nounwind alwaysinline "thunk"
{
  call %mzone* @llvm_zone_reset_extern(%mzone* %zone)
  ret %mzone* %zone
}

declare %mzone* @llvm_zone_create_extern(i64) nounwind
define private %mzone* @llvm_zone_create(i64 %size) nounwind alwaysinline "thunk"
{
  %zone = call %mzone* @llvm_zone_create_extern(i64 %size)
  ret %mzone* %zone
}
