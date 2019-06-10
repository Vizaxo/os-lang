(defmacro id (a) (eval a))
(defmacro clearReg (reg) (list (quote xor) reg reg))
(defmacro if (cond true false)
  (list
   cond
   (asm jnz ifFalse)
   true
   (asm jmp ifEnd)
   (asm ifFalse)
   false
   (asm ifEnd)))

(if (asm cmp eax ebx)
    (clearReg (quote eax))
  (clearReg (quote ebx)))

(gensym)
(gensym)
(gensym)
