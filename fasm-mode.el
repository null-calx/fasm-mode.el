;;; fasm-mode.el --- FASM major mode

;; This is free and unencumbered software released into the public domain.

;; Author: calx
;; URL: https://github.com/null-calx/fasm-mode.el
;; URL: https://istaroth.xyz/files/fasm-mode.el
;; Version: 1.0.0

;;; Commentary:

;; A major mode for editing FASM assembly programs. It includes syntax
;; highlighting, and automatic indentation. This software takes heavy
;; insipiration from `nasm-mode'.

;; References and resources, used:
;; 1. https://github.com/skeeto/nasm-mode
;; 2. emacs and elisp manual
;; 3. https://www.omarpolo.com/post/writing-a-major-mode.html
;; 4. https://github.com/the-little-language-designer/fasm-mode (only
;;    for keyword, instruction, directive, etc. list)

;; FASM Home: https://flatassembler.net/

;;; Code:

(defgroup fasm-mode ()
  "Options for `fasm-mode'."
  :group 'languages)

(defgroup fasm-mode-faces ()
  "Faces used by `fasm-mode'."
  :group 'fasm-mode)

(defcustom fasm-basic-offset (default-value 'tab-width)
  "Indentation level for `fasm-mode'."
  :type 'integer
  :group 'fasm-mode)

(defcustom fasm-bracket-offset 4
  "Indentation level for curly brackets in `fasm-mode'."
  :type 'integer
  :group 'fasm-mode)

(defcustom fasm-after-mnemonic-whitespace :tab
  "In `fasm-mode', determines the whitespace to use after mnemonics.
This can be :tab, :space, or nil (do nothing)."
  :type '(choice (const :tab) (const :space) (const nil))
  :group 'fasm-mode)

(defcustom fasm-comment-column (default-value 'comment-column)
  "Column to indent right-margin comments to in `fasm-mode'."
  :type 'integer
  :group 'fasm-mode)

(defface fasm-registers
  '((t :inherit (font-lock-variable-name-face)))
  "Face for registers."
  :group 'fasm-mode-faces)

(defface fasm-types
  '((t :inherit (font-lock-type-face)))
  "Face for types."
  :group 'fasm-mode-faces)

(defface fasm-instructions
  '((t :inherit (font-lock-builtin-face)))
  "Face for instructions."
  :group 'fasm-mode-faces)

(defface fasm-directives
  '((t :inherit (font-lock-keyword-face)))
  "Face for directives."
  :group 'fasm-mode-faces)

(defface fasm-preprocessor
  '((t :inherit (font-lock-preprocessor-face)))
  "Face for preprocessor directives."
  :group 'fasm-mode-faces)

(defface fasm-labels
  '((t :inherit (font-lock-function-name-face)))
  "Face for nonlocal labels."
  :group 'fasm-mode-faces)

(defface fasm-macro-name
  '((t :inherit (font-lock-function-name-face)))
  "Face for nonlocal labels."
  :group 'fasm-mode-faces)

(defface fasm-constant-name
  '((t :inherit (font-lock-variable-name-face)))
  "Face for constant names"
  :group 'fasm-mode-faces)

(defface fasm-constant
  '((t :inherit (font-lock-constant-face)))
  "Face for constant."
  :group 'fasm-mode-faces)

(eval-and-compile
  (defconst fasm--type-list
    '("xx"
      "xxxx"
      "preserve"
      "byte" "word" "dword" "fword" "pword" "qword" "tbyte" "tword"
      "dqword" "xword" "qqword" "yword" "db" "rb" "dw" "du" "rw" "dd" "rd"
      "df" "dp" "rf" "rp" "dq" "rq" "dt" "rt")
    "Type list for `fasm-mode'."))

(eval-and-compile
  (defconst fasm--directive-list
    '("labeling"
      "mod" "rva" "plt" "align" "as" "at" "defined" "dup" "eq" "eqtype"
      "from" "ptr" "relativeto" "used" "binary" "export" "fixups" "import"
      "native" "static" "console" "dynamic" "efiboot" "linkinfo" "readable"
      "resource" "writable" "shareable" "writeable" "efiruntime"
      "executable" "linkremove" "discardable" "interpreter" "notpageable"

      "if"
      "end"
      "tail_call"
      "finish" ;; as end

      "literal"
      "address"
      "branch"
      "false?branch"

      "err" "org" "data" "else" "heap" "load" "align" "break"
      "entry" "extrn" "label" "stack" "store" "times" "while" "assert"
      "format" "public" "repeat" "display" "section" "segment" "virtual"
      "file")
    "Directive and operator list for `fasm-mode'."))

(eval-and-compile
  (defconst fasm--pp-directive-list
    '("define" "include" "irp" "irps" "macro" "match" "purge" "rept"
      "restore" "restruc" "struc" "common" "forward" "local" "reverse"
      "equ" "fix")
    "Preprocessor directive list for `fasm-mode'."))

(eval-and-compile
  (defconst fasm--register-list
    '("al" "bl" "cl" "dl" "spl" "bpl" "sil" "dil" "r8b" "r9b" "r10b" "r11b"
      "r12b" "r13b" "r14b" "r15b" "ah" "bh" "ch" "dh" "ax" "bx" "cx" "dx"
      "sp" "bp" "si" "di" "r8w" "r9w" "r10w" "r11w" "r12w" "r13w" "r14w"
      "r15w" "eax" "ebx" "ecx" "edx" "esp" "ebp" "esi" "edi" "r8d" "r9d"
      "r10d" "r11d" "r12d" "r13d" "r14d" "r15d"

      "rax" "rbx" "rcx" "rdx" "rsp" "rbp" "rsi" "rdi"
      "r8" "r9" "r10" "r11" "r12" "r13" "r14" "r15"

      ;; "xax" "xbx" "xcx" "xdx" "xsp" "xbp" "xsi" "xdi"
      ;; "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15"

      "rip" "es" "cs" "ss" "ds" "fs" "gs" "cr0" "cr2" "cr3" "cr4" "dr0"
      "dr1" "dr2" "dr3" "st0" "st1" "st2" "st3" "st4" "st5" "st6" "st7"
      "mm0" "mm1" "mm2" "mm3" "mm4" "mm5" "mm6" "mm7" "xmm0" "xmm1" "xmm2"
      "xmm3" "xmm4" "xmm5" "xmm6" "xmm7" "xmm8" "xmm9" "xmm10" "xmm11"
      "xmm12" "xmm13" "xmm14" "xmm15" "ymm0" "ymm1" "ymm2" "ymm3" "ymm4"
      "ymm5" "ymm6" "ymm7" "ymm8" "ymm9" "ymm10" "ymm11" "ymm12" "ymm13"
      "ymm14" "ymm15")
    "Register list for `fasm-mode'."))

(eval-and-compile
  (defconst fasm--instruction-list
    '("bt" "in" "ja" "jb" "jc" "je" "jg" "jl" "jo" "jp" "js" "jz" "or"
      "aaa" "aad" "aam" "aas" "adc" "add" "and" "bsf" "bsr" "btc" "btr"
      "bts" "cbw" "cdq" "clc" "cld" "cli" "cmc" "cmp" "cqo" "cwd" "daa"
      "das" "dec" "div" "fld" "fst" "hlt" "inc" "ins" "int" "jae" "jbe"
      "jge" "jle" "jmp" "jna" "jnb" "jnc" "jne" "jng" "jnl" "jno" "jnp"
      "jns" "jnz" "jpe" "jpo" "lar" "lds" "lea" "les" "lfs" "lgs" "lsl"
      "lss" "ltr" "mov" "mul" "neg" "nop" "not" "out" "pop" "por" "rcl"
      "rcr" "rep" "ret" "rol" "ror" "rsm" "sal" "sar" "sbb" "shl" "shr"
      "stc" "std" "sti" "str" "sub" "ud2" "xor" "adcx" "adox" "andn" "arpl"
      "blci" "blcs" "blsi" "blsr" "bzhi" "call" "cdqe" "clac" "clgi" "clts"
      "cmps" "cwde" "dppd" "dpps" "emms" "fabs" "fadd" "fbld" "fchs" "fcom"
      "fcos" "fdiv" "feni" "fild" "fist" "fld1" "fldz" "fmul" "fnop" "fsin"
      "fstp" "fsub" "ftst" "fxam" "fxch" "idiv" "imul" "insb" "insd" "insw"
      "int1" "int3" "into" "invd" "iret" "jcxz" "jnae" "jnbe" "jnge" "jnle"
      "lahf" "lgdt" "lidt" "lldt" "lmsw" "lock" "lods" "loop" "movd" "movq"
      "movs" "mulx" "orpd" "orps" "outs" "pand" "pdep" "pext" "popa" "popd"
      "popf" "popq" "popw" "push" "pxor" "repe" "repz" "retd" "retf" "retn"
      "retq" "retw" "rorx" "sahf" "salc" "sarx" "scas" "seta" "setb" "setc"
      "sete" "setg" "setl" "seto" "setp" "sets" "setz" "sgdt" "shld" "shlx"
      "shrd" "shrx" "sidt" "sldt" "smsw" "stac" "stgi" "stos" "test" "verr"
      "verw" "vpor" "wait" "xadd" "xchg" "xend" "xlat" "addpd" "addps"
      "addsd" "addss" "andpd" "andps" "bextr" "blcic" "blsic" "bound"
      "bswap" "cmova" "cmovb" "cmovc" "cmove" "cmovg" "cmovl" "cmovo"
      "cmovp" "cmovs" "cmovz" "cmppd" "cmpps" "cmpsb" "cmpsd" "cmpsq"
      "cmpss" "cmpsw" "cpuid" "crc32" "divpd" "divps" "divsd" "divss"
      "enter" "extrq" "f2xm1" "faddp" "fbstp" "fclex" "fcomi" "fcomp"
      "fdisi" "fdivp" "fdivr" "femms" "ffree" "fiadd" "ficom" "fidiv"
      "fimul" "finit" "fistp" "fisub" "fldcw" "fldpi" "fmulp" "fneni"
      "fprem" "fptan" "fsave" "fsqrt" "fstcw" "fstsw" "fsubp" "fsubr"
      "fucom" "fwait" "fyl2x" "icebp" "iretd" "iretq" "iretw" "jecxz"
      "jrcxz" "lddqu" "leave" "lodsb" "lodsd" "lodsq" "lodsw" "loopd"
      "loope" "loopq" "loopw" "loopz" "lzcnt" "maxpd" "maxps" "maxsd"
      "maxss" "minpd" "minps" "minsd" "minss" "movbe" "movsb" "movsd"
      "movsq" "movss" "movsw" "movsx" "movzx" "mulpd" "mulps" "mulsd"
      "mulss" "mwait" "outsb" "outsd" "outsw" "pabsb" "pabsd" "pabsw"
      "paddb" "paddd" "paddq" "paddw" "pandn" "pause" "pavgb" "pavgw"
      "pf2id" "pf2iw" "pfacc" "pfadd" "pfmax" "pfmin" "pfmul" "pfrcp"
      "pfsub" "pi2fd" "pi2fw" "popad" "popaw" "popfd" "popfq" "popfw"
      "pslld" "psllq" "psllw" "psrad" "psraw" "psrld" "psrlq" "psrlw"
      "psubb" "psubd" "psubq" "psubw" "ptest" "pusha" "pushd" "pushf"
      "pushq" "pushw" "rcpps" "rcpss" "rdmsr" "rdpmc" "rdtsc" "repne"
      "repnz" "retfd" "retfq" "retfw" "retnd" "retnq" "retnw" "scasb"
      "scasd" "scasq" "scasw" "setae" "setbe" "setge" "setle" "setna"
      "setnb" "setnc" "setne" "setng" "setnl" "setno" "setnp" "setns"
      "setnz" "setpe" "setpo" "stosb" "stosd" "stosq" "stosw" "subpd"
      "subps" "subsd" "subss" "tzcnt" "tzmsk" "vdppd" "vdpps" "vmovd"
      "vmovq" "vmrun" "vmxon" "vorpd" "vorps" "vpand" "vpxor" "wrmsr"
      "xlatb" "xorpd" "xorps" "xsave" "xtest" "aesdec" "aesenc" "aesimc"
      "andnpd" "andnps" "blcmsk" "blsmsk" "cmovae" "cmovbe" "cmovge"
      "cmovle" "cmovna" "cmovnb" "cmovnc" "cmovne" "cmovng" "cmovnl"
      "cmovno" "cmovnp" "cmovns" "cmovnz" "cmovpe" "cmovpo" "comisd"
      "comiss" "fcmovb" "fcmove" "fcmovu" "fcomip" "fcompp" "fdivrp"
      "ffreep" "ficomp" "fidivr" "fisttp" "fisubr" "fldenv" "fldl2e"
      "fldl2t" "fldlg2" "fldln2" "fnclex" "fndisi" "fninit" "fnsave"
      "fnstcw" "fnstsw" "fpatan" "fprem1" "frstor" "frstpm" "fsaved"
      "fsavew" "fscale" "fsetpm" "fstenv" "fsubrp" "fucomi" "fucomp"
      "fxsave" "getsec" "haddpd" "haddps" "hsubpd" "hsubps" "invept"
      "invlpg" "lfence" "llwpcb" "looped" "loopeq" "loopew" "loopne"
      "loopnz" "loopzd" "loopzq" "loopzw" "lwpins" "lwpval" "mfence"
      "movapd" "movaps" "movdqa" "movdqu" "movhpd" "movhps" "movlpd"
      "movlps" "movnti" "movntq" "movsxd" "movupd" "movups" "paddsb"
      "paddsw" "pextrb" "pextrd" "pextrq" "pextrw" "pfnacc" "pfsubr"
      "phaddd" "phaddw" "phsubd" "phsubw" "pinsrb" "pinsrd" "pinsrq"
      "pinsrw" "pmaxsb" "pmaxsd" "pmaxsw" "pmaxub" "pmaxud" "pmaxuw"
      "pminsb" "pminsd" "pminsw" "pminub" "pminud" "pminuw" "pmuldq"
      "pmulhw" "pmulld" "pmullw" "popcnt" "psadbw" "pshufb" "pshufd"
      "pshufw" "psignb" "psignd" "psignw" "pslldq" "psrldq" "psubsb"
      "psubsw" "pswapd" "pushad" "pushaw" "pushfd" "pushfq" "pushfw"
      "rdmsrq" "rdrand" "rdseed" "rdtscp" "setalc" "setnae" "setnbe"
      "setnge" "setnle" "sfence" "shufpd" "shufps" "skinit" "slwpcb"
      "sqrtpd" "sqrtps" "sqrtsd" "sqrtss" "swapgs" "sysret" "t1mskc"
      "vaddpd" "vaddps" "vaddsd" "vaddss" "vandpd" "vandps" "vcmppd"
      "vcmpps" "vcmpsd" "vcmpss" "vdivpd" "vdivps" "vdivsd" "vdivss"
      "vlddqu" "vmaxpd" "vmaxps" "vmaxsd" "vmaxss" "vmcall" "vminpd"
      "vminps" "vminsd" "vminss" "vmload" "vmovsd" "vmovss" "vmread"
      "vmsave" "vmulpd" "vmulps" "vmulsd" "vmulss" "vmxoff" "vpabsb"
      "vpabsd" "vpabsw" "vpaddb" "vpaddd" "vpaddq" "vpaddw" "vpandn"
      "vpavgb" "vpavgw" "vpcmov" "vpcomb" "vpcomd" "vpcomq" "vpcomw"
      "vpermd" "vpermq" "vpperm" "vprotb" "vprotd" "vprotq" "vprotw"
      "vpshab" "vpshad" "vpshaq" "vpshaw" "vpshlb" "vpshld" "vpshlq"
      "vpshlw" "vpslld" "vpsllq" "vpsllw" "vpsrad" "vpsraw" "vpsrld"
      "vpsrlq" "vpsrlw" "vpsubb" "vpsubd" "vpsubq" "vpsubw" "vptest"
      "vrcpps" "vrcpss" "vsubpd" "vsubps" "vsubsd" "vsubss" "vxorpd"
      "vxorps" "wbinvd" "wrmsrq" "xabort" "xbegin" "xgetbv" "xrstor"
      "xsetbv" "blcfill" "blendpd" "blendps" "blsfill" "clflush" "cmovnae"
      "cmovnbe" "cmovnge" "cmovnle" "cmpeqpd" "cmpeqps" "cmpeqsd" "cmpeqss"
      "cmplepd" "cmpleps" "cmplesd" "cmpless" "cmpltpd" "cmpltps" "cmpltsd"
      "cmpltss" "cmpxchg" "fcmovbe" "fcmovnb" "fcmovne" "fcmovnu" "fdecstp"
      "fincstp" "fldenvd" "fldenvw" "fnsaved" "fnsavew" "fnstenv" "frndint"
      "frstord" "frstorw" "fsincos" "fstenvd" "fstenvw" "fucomip" "fucompp"
      "fxrstor" "fxtract" "fyl2xp1" "insertq" "invlpga" "invpcid" "invvpid"
      "ldmxcsr" "loopned" "loopneq" "loopnew" "loopnzd" "loopnzq" "loopnzw"
      "monitor" "movddup" "movdq2q" "movhlps" "movlhps" "movntdq" "movntpd"
      "movntps" "movntsd" "movntss" "movq2dq" "mpsadbw" "paddusb" "paddusw"
      "palignr" "pavgusb" "pblendw" "pcmpeqb" "pcmpeqd" "pcmpeqq" "pcmpeqw"
      "pcmpgtb" "pcmpgtd" "pcmpgtq" "pcmpgtw" "pfcmpeq" "pfcmpge" "pfcmpgt"
      "pfpnacc" "pfrsqrt" "phaddsw" "phsubsw" "pmaddwd" "pmulhrw" "pmulhuw"
      "pmuludq" "pshufhw" "pshuflw" "psubusb" "psubusw" "roundpd" "roundps"
      "roundsd" "roundss" "rsqrtps" "rsqrtss" "stmxcsr" "syscall" "sysexit"
      "sysretq" "ucomisd" "ucomiss" "vaesdec" "vaesenc" "vaesimc" "vandnpd"
      "vandnps" "vcomisd" "vcomiss" "vfrczpd" "vfrczps" "vfrczsd" "vfrczss"
      "vhaddpd" "vhaddps" "vhsubpd" "vhsubps" "vmclear" "vmmcall" "vmovapd"
      "vmovaps" "vmovdqa" "vmovdqu" "vmovhpd" "vmovhps" "vmovlpd" "vmovlps"
      "vmovupd" "vmovups" "vmptrld" "vmptrst" "vmwrite" "vpaddsb" "vpaddsw"
      "vpcomub" "vpcomud" "vpcomuq" "vpcomuw" "vpermpd" "vpermps" "vpextrb"
      "vpextrd" "vpextrq" "vpextrw" "vphaddd" "vphaddw" "vphsubd" "vphsubw"
      "vpinsrb" "vpinsrd" "vpinsrq" "vpinsrw" "vpmaxsb" "vpmaxsd" "vpmaxsw"
      "vpmaxub" "vpmaxud" "vpmaxuw" "vpminsb" "vpminsd" "vpminsw" "vpminub"
      "vpminud" "vpminuw" "vpmuldq" "vpmulhw" "vpmulld" "vpmullw" "vpsadbw"
      "vpshufb" "vpshufd" "vpsignb" "vpsignd" "vpsignw" "vpslldq" "vpsllvd"
      "vpsllvq" "vpsravd" "vpsrldq" "vpsrlvd" "vpsrlvq" "vpsubsb" "vpsubsw"
      "vshufpd" "vshufps" "vsqrtpd" "vsqrtps" "vsqrtsd" "vsqrtss" "vtestpd"
      "vtestps" "xsave64" "addsubpd" "addsubps" "blendvpd" "blendvps"
      "cmpneqpd" "cmpneqps" "cmpneqsd" "cmpneqss" "cmpnlepd" "cmpnleps"
      "cmpnlesd" "cmpnless" "cmpnltpd" "cmpnltps" "cmpnltsd" "cmpnltss"
      "cmpordpd" "cmpordps" "cmpordsd" "cmpordss" "cvtdq2pd" "cvtdq2ps"
      "cvtpd2dq" "cvtpd2pi" "cvtpd2ps" "cvtpi2pd" "cvtpi2ps" "cvtps2dq"
      "cvtps2pd" "cvtps2pi" "cvtsd2si" "cvtsd2ss" "cvtsi2sd" "cvtsi2ss"
      "cvtss2sd" "cvtss2si" "fcmovnbe" "fnstenvd" "fnstenvw" "fxsave64"
      "insertps" "maskmovq" "movmskpd" "movmskps" "movntdqa" "movshdup"
      "movsldup" "packssdw" "packsswb" "packusdw" "packuswb" "pblendvb"
      "pfrcpit1" "pfrcpit2" "pfrsqit1" "pmovmskb" "pmovsxbd" "pmovsxbq"
      "pmovsxbw" "pmovsxdq" "pmovsxwd" "pmovsxwq" "pmovzxbd" "pmovzxbq"
      "pmovzxbw" "pmovzxdq" "pmovzxwd" "pmovzxwq" "pmulhrsw" "prefetch"
      "rdfsbase" "rdgsbase" "sysenter" "sysexitq" "unpckhpd" "unpckhps"
      "unpcklpd" "unpcklps" "vblendpd" "vblendps" "vcmpeqpd" "vcmpeqps"
      "vcmpeqsd" "vcmpeqss" "vcmpgepd" "vcmpgeps" "vcmpgesd" "vcmpgess"
      "vcmpgtpd" "vcmpgtps" "vcmpgtsd" "vcmpgtss" "vcmplepd" "vcmpleps"
      "vcmplesd" "vcmpless" "vcmpltpd" "vcmpltps" "vcmpltsd" "vcmpltss"
      "vfmaddpd" "vfmaddps" "vfmaddsd" "vfmaddss" "vfmsubpd" "vfmsubps"
      "vfmsubsd" "vfmsubss" "vldmxcsr" "vmlaunch" "vmovddup" "vmovhlps"
      "vmovlhps" "vmovntdq" "vmovntpd" "vmovntps" "vmpsadbw" "vmresume"
      "vpaddusb" "vpaddusw" "vpalignr" "vpblendd" "vpblendw" "vpcmpeqb"
      "vpcmpeqd" "vpcmpeqq" "vpcmpeqw" "vpcmpgtb" "vpcmpgtd" "vpcmpgtq"
      "vpcmpgtw" "vpcomeqb" "vpcomeqd" "vpcomeqq" "vpcomeqw" "vpcomgeb"
      "vpcomged" "vpcomgeq" "vpcomgew" "vpcomgtb" "vpcomgtd" "vpcomgtq"
      "vpcomgtw" "vpcomleb" "vpcomled" "vpcomleq" "vpcomlew" "vpcomltb"
      "vpcomltd" "vpcomltq" "vpcomltw" "vphaddbd" "vphaddbq" "vphaddbw"
      "vphadddq" "vphaddsw" "vphaddwd" "vphaddwq" "vphsubbw" "vphsubdq"
      "vphsubsw" "vphsubwd" "vpmacsdd" "vpmacswd" "vpmacsww" "vpmaddwd"
      "vpmulhuw" "vpmuludq" "vpshufhw" "vpshuflw" "vpsubusb" "vpsubusw"
      "vroundpd" "vroundps" "vroundsd" "vroundss" "vrsqrtps" "vrsqrtss"
      "vstmxcsr" "vucomisd" "vucomiss" "vzeroall" "wrfsbase" "wrgsbase"
      "xacquire" "xrelease" "xrstor64" "xsaveopt" "cmpxchg8b" "cvttpd2dq"
      "cvttpd2pi" "cvttps2dq" "cvttps2pi" "cvttsd2si" "cvttss2si"
      "extractps" "fxrstor64" "pclmulqdq" "pcmpestri" "pcmpestrm"
      "pcmpistri" "pcmpistrm" "pmaddubsw" "prefetchw" "punpckhbw"
      "punpckhdq" "punpckhwd" "punpcklbw" "punpckldq" "punpcklwd"
      "vaddsubpd" "vaddsubps" "vblendvpd" "vblendvps" "vcmpneqpd"
      "vcmpneqps" "vcmpneqsd" "vcmpneqss" "vcmpngepd" "vcmpngeps"
      "vcmpngesd" "vcmpngess" "vcmpngtpd" "vcmpngtps" "vcmpngtsd"
      "vcmpngtss" "vcmpnlepd" "vcmpnleps" "vcmpnlesd" "vcmpnless"
      "vcmpnltpd" "vcmpnltps" "vcmpnltsd" "vcmpnltss" "vcmpordpd"
      "vcmpordps" "vcmpordsd" "vcmpordss" "vcvtdq2pd" "vcvtdq2ps"
      "vcvtpd2dq" "vcvtpd2ps" "vcvtph2ps" "vcvtps2dq" "vcvtps2pd"
      "vcvtps2ph" "vcvtsd2si" "vcvtsd2ss" "vcvtsi2sd" "vcvtsi2ss"
      "vcvtss2sd" "vcvtss2si" "vfnmaddpd" "vfnmaddps" "vfnmaddsd"
      "vfnmaddss" "vfnmsubpd" "vfnmsubps" "vfnmsubsd" "vfnmsubss"
      "vinsertps" "vmovmskpd" "vmovmskps" "vmovntdqa" "vmovshdup"
      "vmovsldup" "vpackssdw" "vpacksswb" "vpackusdw" "vpackuswb"
      "vpblendvb" "vpcomequb" "vpcomequd" "vpcomequq" "vpcomequw"
      "vpcomgeub" "vpcomgeud" "vpcomgeuq" "vpcomgeuw" "vpcomgtub"
      "vpcomgtud" "vpcomgtuq" "vpcomgtuw" "vpcomleub" "vpcomleud"
      "vpcomleuq" "vpcomleuw" "vpcomltub" "vpcomltud" "vpcomltuq"
      "vpcomltuw" "vpcomneqb" "vpcomneqd" "vpcomneqq" "vpcomneqw"
      "vpermilpd" "vpermilps" "vphaddubd" "vphaddubq" "vphaddubw"
      "vphaddudq" "vphadduwd" "vphadduwq" "vpmacsdqh" "vpmacsdql"
      "vpmacssdd" "vpmacsswd" "vpmacssww" "vpmadcswd" "vpmovmskb"
      "vpmovsxbd" "vpmovsxbq" "vpmovsxbw" "vpmovsxdq" "vpmovsxwd"
      "vpmovsxwq" "vpmovzxbd" "vpmovzxbq" "vpmovzxbw" "vpmovzxdq"
      "vpmovzxwd" "vpmovzxwq" "vpmulhrsw" "vunpckhpd" "vunpckhps"
      "vunpcklpd" "vunpcklps" "aesdeclast" "aesenclast" "cmpunordpd"
      "cmpunordps" "cmpunordsd" "cmpunordss" "cmpxchg16b" "loadall286"
      "loadall386" "maskmovdqu" "phminposuw" "prefetcht0" "prefetcht1"
      "prefetcht2" "punpckhqdq" "punpcklqdq" "vcmptruepd" "vcmptrueps"
      "vcmptruesd" "vcmptruess" "vcvttpd2dq" "vcvttps2dq" "vcvttsd2si"
      "vcvttss2si" "vextractps" "vgatherdpd" "vgatherdps" "vgatherqpd"
      "vgatherqps" "vmaskmovpd" "vmaskmovps" "vpclmulqdq" "vpcmpestri"
      "vpcmpestrm" "vpcmpistri" "vpcmpistrm" "vpcomnequb" "vpcomnequd"
      "vpcomnequq" "vpcomnequw" "vpcomtrueb" "vpcomtrued" "vpcomtrueq"
      "vpcomtruew" "vperm2f128" "vperm2i128" "vpermil2pd" "vpermil2ps"
      "vpgatherdd" "vpgatherdq" "vpgatherqd" "vpgatherqq" "vpmacssdqh"
      "vpmacssdql" "vpmadcsswd" "vpmaddubsw" "vpmaskmovd" "vpmaskmovq"
      "vpunpckhbw" "vpunpckhdq" "vpunpckhwd" "vpunpcklbw" "vpunpckldq"
      "vpunpcklwd" "vzeroupper" "xsaveopt64" "pclmulhqhdq" "pclmullqhdq"
      "prefetchnta" "vaesdeclast" "vaesenclast" "vcmpeq_ospd" "vcmpeq_osps"
      "vcmpeq_ossd" "vcmpeq_osss" "vcmpeq_uqpd" "vcmpeq_uqps" "vcmpeq_uqsd"
      "vcmpeq_uqss" "vcmpeq_uspd" "vcmpeq_usps" "vcmpeq_ussd" "vcmpeq_usss"
      "vcmpfalsepd" "vcmpfalseps" "vcmpfalsesd" "vcmpfalsess" "vcmpge_oqpd"
      "vcmpge_oqps" "vcmpge_oqsd" "vcmpge_oqss" "vcmpgt_oqpd" "vcmpgt_oqps"
      "vcmpgt_oqsd" "vcmpgt_oqss" "vcmple_oqpd" "vcmple_oqps" "vcmple_oqsd"
      "vcmple_oqss" "vcmplt_oqpd" "vcmplt_oqps" "vcmplt_oqsd" "vcmplt_oqss"
      "vcmpord_spd" "vcmpord_sps" "vcmpord_ssd" "vcmpord_sss" "vcmpunordpd"
      "vcmpunordps" "vcmpunordsd" "vcmpunordss" "vfmadd132pd" "vfmadd132ps"
      "vfmadd132sd" "vfmadd132ss" "vfmadd213pd" "vfmadd213ps" "vfmadd213sd"
      "vfmadd213ss" "vfmadd231pd" "vfmadd231ps" "vfmadd231sd" "vfmadd231ss"
      "vfmaddsubpd" "vfmaddsubps" "vfmsub132pd" "vfmsub132ps" "vfmsub132sd"
      "vfmsub132ss" "vfmsub213pd" "vfmsub213ps" "vfmsub213sd" "vfmsub213ss"
      "vfmsub231pd" "vfmsub231ps" "vfmsub231sd" "vfmsub231ss" "vfmsubaddpd"
      "vfmsubaddps" "vinsertf128" "vinserti128" "vmaskmovdqu" "vpcomfalseb"
      "vpcomfalsed" "vpcomfalseq" "vpcomfalsew" "vpcomtrueub" "vpcomtrueud"
      "vpcomtrueuq" "vpcomtrueuw" "vphminposuw" "vpunpckhqdq" "vpunpcklqdq"
      "pclmulhqhqdq" "pclmulhqlqdq" "pclmullqhqdq" "pclmullqlqdq"
      "vbroadcastsd" "vbroadcastss" "vcmpneq_oqpd" "vcmpneq_oqps"
      "vcmpneq_oqsd" "vcmpneq_oqss" "vcmpneq_ospd" "vcmpneq_osps"
      "vcmpneq_ossd" "vcmpneq_osss" "vcmpneq_uspd" "vcmpneq_usps"
      "vcmpneq_ussd" "vcmpneq_usss" "vcmpnge_uqpd" "vcmpnge_uqps"
      "vcmpnge_uqsd" "vcmpnge_uqss" "vcmpngt_uqpd" "vcmpngt_uqps"
      "vcmpngt_uqsd" "vcmpngt_uqss" "vcmpnle_uqpd" "vcmpnle_uqps"
      "vcmpnle_uqsd" "vcmpnle_uqss" "vcmpnlt_uqpd" "vcmpnlt_uqps"
      "vcmpnlt_uqsd" "vcmpnlt_uqss" "vextractf128" "vextracti128"
      "vfnmadd132pd" "vfnmadd132ps" "vfnmadd132sd" "vfnmadd132ss"
      "vfnmadd213pd" "vfnmadd213ps" "vfnmadd213sd" "vfnmadd213ss"
      "vfnmadd231pd" "vfnmadd231ps" "vfnmadd231sd" "vfnmadd231ss"
      "vfnmsub132pd" "vfnmsub132ps" "vfnmsub132sd" "vfnmsub132ss"
      "vfnmsub213pd" "vfnmsub213ps" "vfnmsub213sd" "vfnmsub213ss"
      "vfnmsub231pd" "vfnmsub231ps" "vfnmsub231sd" "vfnmsub231ss"
      "vpbroadcastb" "vpbroadcastd" "vpbroadcastq" "vpbroadcastw"
      "vpclmulhqhdq" "vpclmullqhdq" "vpcomfalseub" "vpcomfalseud"
      "vpcomfalseuq" "vpcomfalseuw" "vpermilmo2pd" "vpermilmo2ps"
      "vpermilmz2pd" "vpermilmz2ps" "vpermiltd2pd" "vpermiltd2ps"
      "vcmptrue_uspd" "vcmptrue_usps" "vcmptrue_ussd" "vcmptrue_usss"
      "vcmpunord_spd" "vcmpunord_sps" "vcmpunord_ssd" "vcmpunord_sss"
      "vpclmulhqlqdq" "vpclmullqlqdq" "vbroadcastf128" "vbroadcasti128"
      "vcmpfalse_ospd" "vcmpfalse_osps" "vcmpfalse_ossd" "vcmpfalse_osss"
      "vfmaddsub132pd" "vfmaddsub132ps" "vfmaddsub213pd" "vfmaddsub213ps"
      "vfmaddsub231pd" "vfmaddsub231ps" "vfmsubadd132pd" "vfmsubadd132ps"
      "vfmsubadd213pd" "vfmsubadd213ps" "vfmsubadd231pd" "vfmsubadd231ps"
      "aeskeygenassist" "vaeskeygenassist")
    "Instruction list for `fasm-mode'."))

(defmacro fasm--opt (keywords)
  "Prepare KEYWORDS for `looking-at'."
  `(eval-when-compile
     (regexp-opt ,keywords 'words)))

;; TODO: check if the number regexp match fasm syntax
;; TODO: check for signs too
(defconst fasm--number-binary-regexp
  "\\_<[01]+b\\_>"
  "Regexp for matching binary numbers in `fasm-mode'.")

(defconst fasm--number-hexadecimal-1-regexp
  "\\_<[0-9][0-9a-fA-F]*h\\_>"
  "Regexp for matching hexadecimal numbers (type 1) in `fasm-mode'.")

(defconst fasm--number-hexadecimal-2-regexp
  "\\_<\\(?:0x\\|\\$\\)[0-9a-fA-F]+\\_>"
  "Regexp for matching hexadecimal numbers (type 2) in `fasm-mode'.")

(defconst fasm--number-decimal-regexp
  "\\_<[0-9]+\\(?:\\.[0-9]*\\)?\\(?:e[+-]?[0-9]+\\)?\\_>"
  "Regexp for matching decimal numbers in `fasm-mode'.")

(defconst fasm--number-regexp
  (concat fasm--number-binary-regexp "\\|"
	  fasm--number-hexadecimal-1-regexp "\\|"
	  fasm--number-hexadecimal-2-regexp "\\|"
	  fasm--number-decimal-regexp)
  "Regexp for matching numbers in `fasm-mode'.")

;; TODO: check all other regexp
(defconst fasm--label-regexp
  "\\([a-z$A-Z0-9.?!@]\\(?:\\sw\\|\\s_\\)*\\):"
  "Regexp for matching labels in `fasm-mode'.")

(defconst fasm--macro-name-regexp
  "\\(?:macro\\|struc\\)[ \t]+\\([a-zA-Z0-9.?!@]\\(?:\\sw\\|\\s_\\)*\\)"
  "Regexp for matching macro names in `fasm-mode'.")

;; TODO: not sure about this regexp, because it's not copied
;; TODO: add other constant definition
(defconst fasm--constant-name-regexp
  "\\([a-zA-Z0-9_$]+\\)[ \t]*\\(=\\|db\\|equ\\)"
  "Regexp for matching constant names in `fasm-mode'.")

(defconst fasm-font-lock-keywords
  `((,fasm--number-regexp 			. 'fasm-constant)
    (,fasm--label-regexp			. (1 'fasm-labels))
    (,fasm--macro-name-regexp			. (1 'fasm-macro-name))
    (,fasm--constant-name-regexp		. (1 'fasm-constant-name))
    (,(fasm--opt fasm--type-list)		. 'fasm-types)
    (,(fasm--opt fasm--register-list) 		. 'fasm-registers)
    (,(fasm--opt fasm--instruction-list)	. 'fasm-instructions)
    (,(fasm--opt fasm--directive-list) 		. 'fasm-directives)
    (,(fasm--opt fasm--pp-directive-list)	. 'fasm-preprocessor))
  "Font lock keywords for `fasm-mode'.")

(defconst fasm-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; TODO: correct syntax table
    ;; symbol
    (modify-syntax-entry ?_ "_")
    (modify-syntax-entry ?. "w")
    ;; operators
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?- ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?\\ ".")
    ;; comments
    (modify-syntax-entry ?\; "<")
    (modify-syntax-entry ?\n ">")
    ;; strings
    (modify-syntax-entry ?\" "\"")
    (modify-syntax-entry ?\' "\"")
    ;; curly brackets
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} "){")
    ;; return the syntax table
    (syntax-table))
  "Syntax table for `fasm-mode'.")

(defvar fasm-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd ":") #'fasm-colon)
      (define-key map (kbd ";") #'fasm-comment)))
  "Key bindings for `fasm-mode'.")

(defun fasm-colon ()
  "Indent after inserting a colon and converting the current line
into a label."
  (interactive)
  (call-interactively #'self-insert-command)
  (fasm-indent-line))

(defun fasm-comment (&optional kill)
  "Begin a comment.

If the line has code, indent first. With a prefix arg, kill the
comment on the current line with `comment-kill'."
  (interactive "p")
  (cond
   ;; have a prefix arg? kill comment
   ((not (eql kill 1))
    (comment-kill nil))
   ;; empty line, or inside an indentation, string or comment? insert
   ((or (fasm--empty-line-p)
	(fasm--inside-indentation-p)
	(nth 3 (syntax-ppss))
	(nth 4 (syntax-ppss))) ; TODO: use `syntax-ppss' once only
    (insert ";"))
   ;; line has code? jump to right-side and insert
   ((fasm--line-has-non-comment-p)
    (comment-indent))
   ;; otherwise insert
   ((insert ";"))))

;; TODO: this functino doesn't make sense
(defun fasm-indent-line ()
  "Indent current line (or insert a tab) as FASM assembly code.
This will be called by `indent-for-tab-command' when TAB is
pressed. We indent the entire line as appropriate whenever POINT
is not immediately after a mnemonic; otherwise, we insert a tab."
  (interactive)
  (let ((length-from-behind (- (point-max) (point))))
    (cond
     ;; should indent line to zero
     ((save-excursion
	(back-to-indentation)
	(or (looking-at ";;+")
	    (looking-at fasm--label-regexp)
	    (looking-at fasm--constant-name-regexp)
	    (looking-at (fasm--opt fasm--directive-list))
	    (looking-at (fasm--opt fasm--pp-directive-list))))
      (indent-line-to 0))
     ;; half-indent curly brackets
     ((save-excursion
	(back-to-indentation)
	(or (looking-at "{")
	    (looking-at "}")))
      (indent-line-to fasm-bracket-offset))
     ;; jump to offset
     ((and (not (fasm--empty-line-p))
	   (not (fasm--inside-indentation-p))
	   (not (nth 4 (syntax-ppss))))
      (cl-case fasm-after-mnemonic-whitespace
	(:tab   (insert "\t"))
	(:space (insert-char ?\s fasm-basic-offset))))
     ;; otherwise indent to basic-offset
     ((indent-line-to fasm-basic-offset)))
    (when (> (- (point-max) length-from-behind) (point))
      (goto-char (- (point-max) length-from-behind)))))

(defun fasm--current-line ()
  "Return the current line as a string."
  (save-excursion
    (let ((start (progn (beginning-of-line) (point)))
	  (end (progn (end-of-line) (point))))
      (buffer-substring-no-properties start end))))

(defun fasm--empty-line-p ()
  "Return non-nil if current line has non-whitespace."
  (not (string-match-p "\\S-" (fasm--current-line))))

(defun fasm--inside-indentation-p ()
  "Return non-nil if point is within the indentation."
  (save-excursion
    (let ((point (point))
	  (start (progn (beginning-of-line) (point)))
	  (end (progn (back-to-indentation) (point))))
      (and (<= start point) (<= point end)))))

(defun fasm--line-has-non-comment-p ()
  "Return non-nil if current line has code."
  (let* ((line (fasm--current-line))
	 (match (string-match-p "\\S-" line)))
    (when match
      (not (eql ?\; (aref line match))))))

(defun fasm-comment-indent ()
  "Compute desired indentation for comment on the current line."
  fasm-comment-column)

(defun fasm-insert-comment ()
  "Insert a comment if the current line doesn't contain one."
  (let ((comment-insert-comment-function nil))
    (comment-indent)))

;;;###autoload
(define-derived-mode fasm-mode prog-mode "FASM"
  "Major mode for editing FASM assembly programs."
  :group 'fasm-mode
  (make-local-variable 'comment-start)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'comment-insert-comment-function)
  (setf comment-start ";"
	font-lock-defaults '(fasm-font-lock-keywords nil :case-fold)
	indent-line-function #'fasm-indent-line
	comment-indent-function #'fasm-comment-indent
	comment-insert-comment-function #'fasm-insert-comment))

(provide 'fasm-mode)

;;; fasm-mode.el ends here
