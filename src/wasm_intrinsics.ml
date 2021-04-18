let memcpy_funcs = {|
(func $memcpy_sec_sec untrusted (param $src i32) (param $dest i32) (param $nwords i32)
(local $curr i32)
(local $end i32)

(set_local $curr (i32.const 0))
(set_local $end (i32.mul (get_local $nwords) (i32.const 8)))

(block
    (loop
        (br_if 1 (i32.ge_u (get_local $curr) (get_local $end)))
        (s64.store 0 (i32.add (get_local $dest) (get_local $curr)) (s64.load 0 (i32.add (get_local $src) (get_local $curr))))

        (set_local $curr (i32.add (get_local $curr) (i32.const 8)))
        (br 0)
    )
)
)

(func $memcpy_pub_pub untrusted (param $src i32) (param $dest i32) (param $nwords i32)
(local $curr i32)
(local $end i32)

(set_local $curr (i32.const 0))
(set_local $end (i32.mul (get_local $nwords) (i32.const 8)))

(block
    (loop
        (br_if 1 (i32.ge_u (get_local $curr) (get_local $end)))
        (i64.store 1 (i32.add (get_local $dest) (get_local $curr)) (i64.load 1 (i32.add (get_local $src) (get_local $curr))))

        (set_local $curr (i32.add (get_local $curr) (i32.const 8)))
        (br 0)
    )
)
)

(func $memcpy_pub_sec untrusted (param $src i32) (param $dest i32) (param $nwords i32)
(local $curr i32)
(local $end i32)

(set_local $curr (i32.const 0))
(set_local $end (i32.mul (get_local $nwords) (i32.const 8)))

(block
    (loop
        (br_if 1 (i32.ge_u (get_local $curr) (get_local $end)))
        (s64.store 0 (i32.add (get_local $dest) (get_local $curr)) (s64.classify (i64.load 1 (i32.add (get_local $src) (get_local $curr)))))

        (set_local $curr (i32.add (get_local $curr) (i32.const 8)))
        (br 0)
    )
)
)

(func $memcpy_sec_pub trusted (param $src i32) (param $dest i32) (param $nwords i32)
(local $curr i32)
(local $end i32)

(set_local $curr (i32.const 0))
(set_local $end (i32.mul (get_local $nwords) (i32.const 8)))

(block
    (loop
        (br_if 1 (i32.ge_u (get_local $curr) (get_local $end)))
        (i64.store 1 (i32.add (get_local $dest) (get_local $curr)) (i64.declassify (s64.load 0 (i32.add (get_local $src) (get_local $curr)))))

        (set_local $curr (i32.add (get_local $curr) (i32.const 8)))
        (br 0)
    )
)
)

(func $bzero_sec untrusted (param $dest i32) (param $nwords i32)
(local $curr i32)
(local $end i32)

(set_local $curr (i32.const 0))
(set_local $end (i32.mul (get_local $nwords) (i32.const 8)))

(block
    (loop
        (br_if 1 (i32.ge_u (get_local $curr) (get_local $end)))
        (s64.store 0 (i32.add (get_local $dest) (get_local $curr)) (s64.const 0))

        (set_local $curr (i32.add (get_local $curr) (i32.const 8)))
        (br 0)
    )
)
)

(func $bzero_pub untrusted (param $dest i32) (param $nwords i32)
(local $curr i32)
(local $end i32)

(set_local $curr (i32.const 0))
(set_local $end (i32.mul (get_local $nwords) (i32.const 8)))

(block
    (loop
        (br_if 1 (i32.ge_u (get_local $curr) (get_local $end)))
        (i64.store 1 (i32.add (get_local $dest) (get_local $curr)) (i64.const 0))

        (set_local $curr (i32.add (get_local $curr) (i32.const 8)))
        (br 0)
    )
)
)
|} |> Str.global_replace (Str.regexp "[\n ]+") " "