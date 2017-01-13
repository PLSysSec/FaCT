; ModuleID = 'Module'

define i32 @ssl3_cbc_remove_padding([1024 x i32]* %data, [1024 x i32]* %input, i32 %block_size, i32 %mac_size, [2 x i32]* %lengthtype_array) {
entry:
  %rval = alloca i32
  store i32 0, i32* %rval
  %rset = alloca i32
  store i32 0, i32* %rset
  %padding_length = alloca i32
  store i32 4, i32* %padding_length
  %good1 = alloca i32
  store i32 0, i32* %good1
  %good2 = alloca i32
  store i32 0, i32* %good2
  %addtmp = add i32 1, %mac_size
  %overhead = alloca i32
  store i32 %addtmp, i32* %overhead
  %overhead1 = load i32* %overhead
  %ptr = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 0
  %p = load i32* %ptr
  %gt = icmp ugt i32 %overhead1, %p
  %gtcmp = sext i1 %gt to i32
  %m1 = alloca i32
  store i32 %gtcmp, i32* %m1
  %rval2 = load i32* %rval
  %m13 = load i32* %m1
  %rset4 = load i32* %rset
  %nottmp = xor i32 %rset4, -1
  %andtmp = and i32 %m13, %nottmp
  %andtmp5 = and i32 0, %andtmp
  %ortmp = or i32 %rval2, %andtmp5
  store i32 %ortmp, i32* %rval
  %rset6 = load i32* %rset
  %m17 = load i32* %m1
  %ortmp8 = or i32 %rset6, %m17
  store i32 %ortmp8, i32* %rset
  %m19 = load i32* %m1
  %nottmp10 = xor i32 %m19, -1
  store i32 %nottmp10, i32* %m1
  %m111 = load i32* %m1
  %rset12 = load i32* %rset
  %nottmp13 = xor i32 %rset12, -1
  %andtmp14 = and i32 %m111, %nottmp13
  %andtmp15 = and i32 0, %andtmp14
  %good116 = load i32* %good1
  %m117 = load i32* %m1
  %rset18 = load i32* %rset
  %nottmp19 = xor i32 %rset18, -1
  %andtmp20 = and i32 %m117, %nottmp19
  %nottmp21 = xor i32 %andtmp20, -1
  %andtmp22 = and i32 %good116, %nottmp21
  %ortmp23 = or i32 %andtmp15, %andtmp22
  store i32 %ortmp23, i32* %good1
  %ptr24 = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 0
  %p25 = load i32* %ptr24
  %padding_length26 = load i32* %padding_length
  %overhead27 = load i32* %overhead
  %addtmp28 = add i32 %padding_length26, %overhead27
  %gte = icmp uge i32 %p25, %addtmp28
  %gtecmp = sext i1 %gte to i32
  %rset29 = load i32* %rset
  %nottmp30 = xor i32 %rset29, -1
  %andtmp31 = and i32 -1, %nottmp30
  %andtmp32 = and i32 %gtecmp, %andtmp31
  %good133 = load i32* %good1
  %rset34 = load i32* %rset
  %nottmp35 = xor i32 %rset34, -1
  %andtmp36 = and i32 -1, %nottmp35
  %nottmp37 = xor i32 %andtmp36, -1
  %andtmp38 = and i32 %good133, %nottmp37
  %ortmp39 = or i32 %andtmp32, %andtmp38
  store i32 %ortmp39, i32* %good1
  %padding_length40 = load i32* %padding_length
  %addtmp41 = add i32 %padding_length40, 1
  %gte42 = icmp uge i32 %block_size, %addtmp41
  %gtecmp43 = sext i1 %gte42 to i32
  %rset44 = load i32* %rset
  %nottmp45 = xor i32 %rset44, -1
  %andtmp46 = and i32 -1, %nottmp45
  %andtmp47 = and i32 %gtecmp43, %andtmp46
  %good248 = load i32* %good2
  %rset49 = load i32* %rset
  %nottmp50 = xor i32 %rset49, -1
  %andtmp51 = and i32 -1, %nottmp50
  %nottmp52 = xor i32 %andtmp51, -1
  %andtmp53 = and i32 %good248, %nottmp52
  %ortmp54 = or i32 %andtmp47, %andtmp53
  store i32 %ortmp54, i32* %good2
  %good155 = load i32* %good1
  %m2 = alloca i32
  store i32 %good155, i32* %m2
  %good256 = load i32* %good2
  %m257 = load i32* %m2
  %andtmp58 = and i32 %good256, %m257
  %m3 = alloca i32
  store i32 %andtmp58, i32* %m3
  %padding_length59 = load i32* %padding_length
  %addtmp60 = add i32 %padding_length59, 1
  %m361 = load i32* %m3
  %m262 = load i32* %m2
  %andtmp63 = and i32 %m361, %m262
  %rset64 = load i32* %rset
  %nottmp65 = xor i32 %rset64, -1
  %andtmp66 = and i32 %andtmp63, %nottmp65
  %andtmp67 = and i32 %addtmp60, %andtmp66
  %padding_length68 = load i32* %padding_length
  %m369 = load i32* %m3
  %m270 = load i32* %m2
  %andtmp71 = and i32 %m369, %m270
  %rset72 = load i32* %rset
  %nottmp73 = xor i32 %rset72, -1
  %andtmp74 = and i32 %andtmp71, %nottmp73
  %nottmp75 = xor i32 %andtmp74, -1
  %andtmp76 = and i32 %padding_length68, %nottmp75
  %ortmp77 = or i32 %andtmp67, %andtmp76
  store i32 %ortmp77, i32* %padding_length
  %m378 = load i32* %m3
  %nottmp79 = xor i32 %m378, -1
  store i32 %nottmp79, i32* %m3
  %m380 = load i32* %m3
  %m281 = load i32* %m2
  %andtmp82 = and i32 %m380, %m281
  %rset83 = load i32* %rset
  %nottmp84 = xor i32 %rset83, -1
  %andtmp85 = and i32 %andtmp82, %nottmp84
  %andtmp86 = and i32 0, %andtmp85
  %padding_length87 = load i32* %padding_length
  %m388 = load i32* %m3
  %m289 = load i32* %m2
  %andtmp90 = and i32 %m388, %m289
  %rset91 = load i32* %rset
  %nottmp92 = xor i32 %rset91, -1
  %andtmp93 = and i32 %andtmp90, %nottmp92
  %nottmp94 = xor i32 %andtmp93, -1
  %andtmp95 = and i32 %padding_length87, %nottmp94
  %ortmp96 = or i32 %andtmp86, %andtmp95
  store i32 %ortmp96, i32* %padding_length
  %m297 = load i32* %m2
  %nottmp98 = xor i32 %m297, -1
  store i32 %nottmp98, i32* %m2
  %m299 = load i32* %m2
  %rset100 = load i32* %rset
  %nottmp101 = xor i32 %rset100, -1
  %andtmp102 = and i32 %m299, %nottmp101
  %andtmp103 = and i32 0, %andtmp102
  %padding_length104 = load i32* %padding_length
  %m2105 = load i32* %m2
  %rset106 = load i32* %rset
  %nottmp107 = xor i32 %rset106, -1
  %andtmp108 = and i32 %m2105, %nottmp107
  %nottmp109 = xor i32 %andtmp108, -1
  %andtmp110 = and i32 %padding_length104, %nottmp109
  %ortmp111 = or i32 %andtmp103, %andtmp110
  store i32 %ortmp111, i32* %padding_length
  %ptr112 = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 0
  %p113 = load i32* %ptr112
  %padding_length114 = load i32* %padding_length
  %subtmp = sub i32 %p113, %padding_length114
  %rset115 = load i32* %rset
  %nottmp116 = xor i32 %rset115, -1
  %andtmp117 = and i32 -1, %nottmp116
  %andtmp118 = and i32 %subtmp, %andtmp117
  %ptr119 = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 0
  %p120 = load i32* %ptr119
  %rset121 = load i32* %rset
  %nottmp122 = xor i32 %rset121, -1
  %andtmp123 = and i32 -1, %nottmp122
  %nottmp124 = xor i32 %andtmp123, -1
  %andtmp125 = and i32 %p120, %nottmp124
  %ortmp126 = or i32 %andtmp118, %andtmp125
  %ptr127 = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 0
  store i32 %ortmp126, i32* %ptr127
  %ptr128 = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 1
  %p129 = load i32* %ptr128
  %padding_length130 = load i32* %padding_length
  %lshift = shl i32 %padding_length130, 8
  %ortmp131 = or i32 %p129, %lshift
  %rset132 = load i32* %rset
  %nottmp133 = xor i32 %rset132, -1
  %andtmp134 = and i32 -1, %nottmp133
  %andtmp135 = and i32 %ortmp131, %andtmp134
  %ptr136 = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 1
  %p137 = load i32* %ptr136
  %rset138 = load i32* %rset
  %nottmp139 = xor i32 %rset138, -1
  %andtmp140 = and i32 -1, %nottmp139
  %nottmp141 = xor i32 %andtmp140, -1
  %andtmp142 = and i32 %p137, %nottmp141
  %ortmp143 = or i32 %andtmp135, %andtmp142
  %ptr144 = getelementptr [2 x i32]* %lengthtype_array, i32 0, i32 1
  store i32 %ortmp143, i32* %ptr144
  %good1145 = load i32* %good1
  %m4 = alloca i32
  store i32 %good1145, i32* %m4
  %good2146 = load i32* %good2
  %m4147 = load i32* %m4
  %andtmp148 = and i32 %good2146, %m4147
  %m5 = alloca i32
  store i32 %andtmp148, i32* %m5
  %rval149 = load i32* %rval
  %m5150 = load i32* %m5
  %m4151 = load i32* %m4
  %andtmp152 = and i32 %m5150, %m4151
  %rset153 = load i32* %rset
  %nottmp154 = xor i32 %rset153, -1
  %andtmp155 = and i32 %andtmp152, %nottmp154
  %andtmp156 = and i32 1, %andtmp155
  %ortmp157 = or i32 %rval149, %andtmp156
  store i32 %ortmp157, i32* %rval
  %rset158 = load i32* %rset
  %m5159 = load i32* %m5
  %m4160 = load i32* %m4
  %andtmp161 = and i32 %m5159, %m4160
  %ortmp162 = or i32 %rset158, %andtmp161
  store i32 %ortmp162, i32* %rset
  %m5163 = load i32* %m5
  %nottmp164 = xor i32 %m5163, -1
  store i32 %nottmp164, i32* %m5
  %rval165 = load i32* %rval
  %m5166 = load i32* %m5
  %m4167 = load i32* %m4
  %andtmp168 = and i32 %m5166, %m4167
  %rset169 = load i32* %rset
  %nottmp170 = xor i32 %rset169, -1
  %andtmp171 = and i32 %andtmp168, %nottmp170
  %andtmp172 = and i32 -1, %andtmp171
  %ortmp173 = or i32 %rval165, %andtmp172
  store i32 %ortmp173, i32* %rval
  %rset174 = load i32* %rset
  %m5175 = load i32* %m5
  %m4176 = load i32* %m4
  %andtmp177 = and i32 %m5175, %m4176
  %ortmp178 = or i32 %rset174, %andtmp177
  store i32 %ortmp178, i32* %rset
  %m4179 = load i32* %m4
  %nottmp180 = xor i32 %m4179, -1
  store i32 %nottmp180, i32* %m4
  %rval181 = load i32* %rval
  %m4182 = load i32* %m4
  %rset183 = load i32* %rset
  %nottmp184 = xor i32 %rset183, -1
  %andtmp185 = and i32 %m4182, %nottmp184
  %andtmp186 = and i32 -1, %andtmp185
  %ortmp187 = or i32 %rval181, %andtmp186
  store i32 %ortmp187, i32* %rval
  %rset188 = load i32* %rset
  %m4189 = load i32* %m4
  %ortmp190 = or i32 %rset188, %m4189
  store i32 %ortmp190, i32* %rset
  %rval191 = load i32* %rval
  ret i32 %rval191
}
