
define internal void @_structcopy(%SHA_CTX* nocapture %dst, %SHA_CTX* nocapture readonly %src) alwaysinline {
entry:
  %_secret_src = bitcast %SHA_CTX* %src to i8*
  %_secret_dst = bitcast %SHA_CTX* %dst to i8*
  tail call void @llvm.memcpy.p0i8.p0i8.i32(i8* %_secret_dst, i8* %_secret_src, i32 96, i32 4, i1 false)
  ret void
}
