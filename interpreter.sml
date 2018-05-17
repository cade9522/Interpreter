fun exists(word : string, binder : (string * (string * string)) list) =
  case binder of
      [] => false
    | _ =>
        let
          val a = hd(binder)
          val b = #1 a
          val c = #2 a
        in
          if word = b then true
          else exists(word, tl(binder))
        end


fun remove(word : string, binder : (string * (string * string)) list) =
  let
    val a = hd(binder)
    val b = #1 a
    val c = #2 a
  in
    if word = b then tl(binder)
    else a::remove(word, tl(binder))
  end


fun lookup(word : string, binder : (string * (string * string)) list) =
  let
    val a = hd(binder)
    val b = #1 a
    val c = #2 a
  in
    if word = b then c
    else lookup(word, tl(binder))
  end


fun canBind(word : string, binder : (string * (string * string)) list) =
  case word of
      "int" => true
    | "string" => true
    | "bool" => true
    | "unit" => true
    | "name" => exists(word, binder)
    | _ => false


fun revNeg(word : string) =
  let
    val a = explode(word)
    val b = hd(a)
  in
    if b = #"~" then true
    else false
  end


fun isNeg(word : string) =
  let
    val a = explode(word)
    val b = hd(a)
  in
    if b = #"-" then true
    else false
  end


fun isInt(word : string) =
  case (Int.fromString word) of
      NONE => false
    | SOME(c) =>
        let
          val num = explode("-1234567890")
          val a = Substring.full word
          val b = Substring.trimr 1 a
          val word2 = Substring.string b
          val list = explode(word2)

          fun check2(letter : char, num : char list) =
            if num = nil then false
            else
              if hd(num) = letter then true
              else check2(letter, tl(num))

          fun check(list : char list, num : char list) =
            if list = nil then true
            else
              if check2(hd(list), num)
              then check(tl(list), num)
              else false

        in
          check(list, num)
        end


fun isString(word : string) =
  let
    val list = explode(word)
    val index = String.size word
    val first = String.sub(word, 0)
    val last = String.sub(word, index - 2)
  in
    if first = #"\"" andalso last = #"\"" then true
    else false
  end


fun isName(word : string) =
  let
    val a = Substring.full word
    val b = Substring.trimr 1 a
    val word2 = Substring.string b
    val list = explode(word2)
    val alphanum = explode("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890")
    val alphabet = explode("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    val first = String.sub(word, 0)

    fun check1(first : char, alphabet : char list) =
      if alphabet = nil then false
      else
        if hd(alphabet) = first then true
        else check1(first, tl(alphabet))

    fun check2(letter : char, alphanum : char list) =
      if alphanum = nil then false
      else
        if hd(alphanum) = letter then true
        else check2(letter, tl(alphanum))

    fun check3(list : char list, alphanum : char list) =
      if list = nil then true
      else
        if check2(hd(list), alphanum)
        then check3(tl(list), alphanum)
        else false

  in
    if check1(first, alphabet) then check3(list, alphanum)
    else false
  end


fun isBool(word : string) =
  case word of
      ":false:\n" => true
    | ":true:\n" => true
    | _ => false

fun ifop(stack : (string * string) list, binder : (string * (string * string)) list) =
  case (List.length stack) of
      0 => (":error:", "err")::stack
    | 1 => (":error:", "err")::stack
    | 2 => (":error:", "err")::stack
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = hd(b)
        val d = tl(b)
        val e = hd(d)
        val f = #1 a
        val g = #1 c
        val h = #1 e
        val i = #2 a
        val j = #2 c
        val k = #2 e

        fun subif(opp : string) =
          if opp = ":true:" then c::tl(d)
          else a::tl(d)

        fun bound1(h : string) =
          case exists(h, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(h, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                in
                  if rep2 = "bool" then subif(rep1)
                  else (":error:", "err")::stack
                end

        fun bound(k : string) =
          if k = "name" then bound1(h)
          else (":error:", "err")::stack

      in
         if k = "bool" then subif(h)
         else bound(k)
      end


fun bind(stack : (string * string) list, binder : (string * (string * string)) list) =
  case (List.length stack) of
      0 => ((":error:", "err")::stack, binder)
    | 1 => ((":error:", "err")::stack, binder)
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = hd(b)
        val d = #1 a
        val e = #1 c
        val f = #2 a
        val g = #2 c

        fun subbind2(name : string, binder : (string * (string * string)) list) =
          let
            val h = (name, a)
            val i = h::binder
            val j = (":unit:", "unit")::tl(b)
          in
            (j, i)
          end

        fun subbind(name : string, binder : (string * (string * string)) list) =
          if exists(name, binder) then subbind2(name, remove(name, binder))
          else subbind2(name, binder)

      in
         if canBind(f, binder) andalso g = "name"
         then subbind(e, binder)
         else ((":error:", "err")::stack, binder)
      end


fun not(stack : (string * string) list, binder : (string * (string * string)) list) =
  case (List.length stack) of
      0 => (":error:", "err")::stack
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = #1 a
        val d = #2 a

        fun return(z : string) = (z, "bool")::b

        fun subnot(x : string) =
          if x = ":true:" then return(":false:")
          else return(":true:")

        fun bound1(c : string) =
          case exists(c, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(c, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                in
                  if rep2 = "bool" then subnot(rep1)
                  else (":error:", "err")::stack
                end

        fun bound(d : string) =
          if d = "name" then bound1(c)
          else (":error:", "err")::stack

      in
        if d = "bool" then subnot(c)
        else bound(d)
      end


fun bool(word : string, stack : (string * string) list, binder : (string * (string * string)) list) =
  case (List.length stack) of
      0 => (":error:", "err")::stack
    | 1 => (":error:", "err")::stack
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = hd(b)
        val d = #1 a
        val e = #1 c
        val f = #2 a
        val g = #2 c

        fun return(z : string) = (z, "bool")::tl(b)

        fun an(x : string, y : string) =
          if x = ":true:" andalso y = ":true:"
          then return(":true:")
          else return(":false:")

        fun or(x : string, y : string) =
          if x = ":true:" then return(":true:")
          else
            if y = ":true:" then return(":true:")
            else return(":false:")

        fun switch(word : string, x : string, y : string)  =
          case word of
              "and" => an(x, y)
            | "or" => or(x, y)

        fun bound3(e : string, d: string) =
          case exists(e, binder) of
              false => (":error:", "err")::stack
            | true =>
                case exists(d, binder) of
                    false => (":error:", "err")::stack
                  | true =>
                      let
                        val rep = lookup(e, binder)
                        val pep = lookup(d, binder)
                        val rep1 = #1 rep
                        val rep2 = #2 rep
                        val pep1 = #1 pep
                        val pep2 = #2 pep
                      in
                        if rep2 = "bool" andalso pep2 = "bool"
                        then switch(word, rep1, pep1)
                        else (":error:", "err")::stack
                      end

        fun bound2(e : string, d: string) =
          case exists(d, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(d, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                in
                  if rep2 = "bool" then switch(word, e, rep1)
                  else (":error:", "err")::stack
                end

        fun bound1(e : string, d: string) =
          case exists(e, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(e, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                in
                  if rep2 = "bool" then switch(word, rep1, d)
                  else (":error:", "err")::stack
                end

        fun bound(f : string, g : string) =
          if f = "bool" andalso g = "name"
          then bound1(e, d)
          else
            if f = "name" andalso g = "bool"
            then bound2(e, d)
            else
              if f = "name" andalso g = "name"
              then bound3(e, d)
              else (":error:", "err")::stack

      in
         if f = "bool" andalso g = "bool"
         then switch(word, e, d)
         else bound(f, g)
      end


fun cat(stack : (string * string) list, binder : (string * (string * string)) list) =
  case (List.length stack) of
      0 => (":error:", "err")::stack
    | 1 => (":error:", "err")::stack
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = hd(b)
        val d = #1 a
        val e = #1 c
        val f = #2 a
        val g = #2 c

        fun subcat(word : string, word2 : string) = String.concat [word, word2]

        fun bound3(e : string, d: string) =
          case exists(e, binder) of
              false => (":error:", "err")::stack
            | true =>
                case exists(d, binder) of
                    false => (":error:", "err")::stack
                  | true =>
                      let
                        val rep = lookup(e, binder)
                        val pep = lookup(d, binder)
                        val rep1 = #1 rep
                        val rep2 = #2 rep
                        val pep1 = #1 pep
                        val pep2 = #2 pep
                      in
                        if rep2 = "string" andalso pep2 = "string"
                        then (subcat(rep1, pep1), "string")::tl(b)
                        else (":error:", "err")::stack
                      end

        fun bound2(e : string, d: string) =
          case exists(d, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(d, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                in
                  if rep2 = "string" then (subcat(e, rep1), "string")::tl(b)
                  else (":error:", "err")::stack
                end

        fun bound1(e : string, d: string) =
          case exists(e, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(e, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                in
                  if rep2 = "string" then (subcat(rep1, d), "string")::tl(b)
                  else (":error:", "err")::stack
                end

        fun bound(f : string, g : string) =
          if f = "string" andalso g = "name"
          then bound1(e, d)
          else
            if f = "name" andalso g = "string"
            then bound2(e, d)
            else
              if f = "name" andalso g = "name"
              then bound3(e, d)
              else (":error:", "err")::stack

      in
        if f = "string" andalso g = "string"
        then (subcat(e, d), "string")::tl(b)
        else bound(f, g)
      end


fun quit(stack : (string * string) list) = stack;


fun swap(stack : (string * string) list) =
  case (List.length stack) of
      0 => (":error:", "err")::stack
    | 1 => (":error:", "err")::stack
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = hd(b)
        val d = tl(b)
        val e = a::d
        val list = c::e
      in
        list
      end


fun neg(stack : (string * string) list, binder : (string * (string * string)) list) =
  case (List.length stack) of
      0 => (":error:", "err")::stack
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = #1 a
        val d = #2 a
        val x = Int.fromString(c)

        fun return(z : int) = (Int.toString(z), "int")::b

        fun subneg(x : int) = return(x * ~1)

        fun bound1(c : string) =
          case exists(c, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(c, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                  val x = Int.fromString(rep1)
                in
                  case x of
                      NONE => (":error:", "err")::stack
                    | SOME(n) =>
                        if rep2 = "int"
                        then subneg(n)
                        else (":error:", "err")::stack
                end

        fun bound(d : string) =
          if d = "name" then bound1(c)
          else (":error:", "err")::stack

      in
        case x of
            NONE => bound(d)
          | SOME(n) =>
              if d = "int"
              then subneg(n)
              else bound(d)
      end


fun math(word : string, stack : (string * string) list, binder : (string * (string * string)) list) =
  case (List.length stack) of
      0 => (":error:", "err")::stack
    | 1 => (":error:", "err")::stack
    | _ =>
      let
        val a = hd(stack)
        val b = tl(stack)
        val c = hd(b)
        val d = #1 a
        val e = #1 c
        val f = #2 a
        val g = #2 c
        val y = Int.fromString(d)
        val x = Int.fromString(e)

        fun return(z : int) = (Int.toString(z), "int")::tl(b)

        fun add(x : int, y : int) = return(x + y)

        fun sub(x : int, y : int) = return(x - y)

        fun mul(x : int, y : int) = return(x * y)

        fun divd(x : int, y : int) =
          case y of
              0 => (":error:", "err")::stack
            | _ => return(x div y)

        fun rem(x : int, y : int) =
          case y of
              0 => (":error:", "err")::stack
            | _ => return(x mod y)

        fun equal(x : int, y : int) =
           if x = y then (":true:", "bool")::tl(b)
           else (":false:", "bool")::tl(b)

        fun lessThan(x : int, y : int) =
           if x < y then (":true:", "bool")::tl(b)
           else (":false:", "bool")::tl(b)

        fun switch(word : string, x : int, y: int)  =
          case word of
              "add" => add(x, y)
            | "sub" => sub(x, y)
            | "mul" => mul(x, y)
            | "div" => divd(x, y)
            | "rem" => rem(x, y)
            | "equal" => equal(x, y)
            | "lessThan" => lessThan(x, y)

        fun bound3(e : string, d: string) =
          case exists(e, binder) of
              false => (":error:", "err")::stack
            | true =>
                case exists(d, binder) of
                    false => (":error:", "err")::stack
                  | true =>
                      let
                        val rep = lookup(e, binder)
                        val pep = lookup(d, binder)
                        val rep1 = #1 rep
                        val rep2 = #2 rep
                        val pep1 = #1 pep
                        val pep2 = #2 pep
                        val y = Int.fromString(pep1)
                        val x = Int.fromString(rep1)
                      in
                        case y of
                           NONE => (":error:", "err")::stack
                         | SOME(n) =>
                             case x of
                                 NONE => (":error:", "err")::stack
                               | SOME(p) =>
                                   if rep2 = "int" andalso pep2 = "int"
                                   then switch(word, p, n)
                                   else (":error:", "err")::stack
                      end

        fun bound2(e : string, d: string) =
          case exists(d, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(d, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                  val y = Int.fromString(rep1)
                in
                  case y of
                     NONE => (":error:", "err")::stack
                   | SOME(n) =>
                       case x of
                           NONE => (":error:", "err")::stack
                         | SOME(p) =>
                             if rep2 = "int" then switch(word, p, n)
                             else (":error:", "err")::stack
                end

        fun bound1(e : string, d: string) =
          case exists(e, binder) of
              false => (":error:", "err")::stack
            | true =>
                let
                  val rep = lookup(e, binder)
                  val rep1 = #1 rep
                  val rep2 = #2 rep
                  val x = Int.fromString(rep1)
                in
                  case y of
                     NONE => (":error:", "err")::stack
                   | SOME(n) =>
                       case x of
                           NONE => (":error:", "err")::stack
                         | SOME(p) =>
                             if rep2 = "int" then switch(word, p, n)
                             else (":error:", "err")::stack
                end

        fun bound(f : string, g : string) =
          if f = "int" andalso g = "name"
          then bound1(e, d)
          else
            if f = "name" andalso g = "int"
            then bound2(e, d)
            else
              if f = "name" andalso g = "name"
              then bound3(e, d)
              else (":error:", "err")::stack

      in
        case y of
            NONE => bound(f, g)
          | SOME(n) =>
              case x of
                  NONE => bound(f, g)
                | SOME(p) =>
                    if f = "int" andalso g = "int"
                    then switch(word, p, n)
                    else bound(f, g)
      end


fun pop(stack : (string * string) list) =
  case stack of
      [] => [(":error:", "err")]
    | _ => tl(stack)


fun pushBool(word : string) =
  let
    val a = Substring.full word
    val b = Substring.trimr 1 a
    val word = Substring.string b
  in
    (word, "bool")
  end


fun pushName(word : string) =
  let
    val a = Substring.full word
    val b = Substring.trimr 1 a
    val word = Substring.string b
  in
    (word, "name")
  end


fun pushString(word : string) =
  let
    val a = Substring.full word
    val b = Substring.triml 1 a
    val c = Substring.trimr 2 b
    val word = Substring.string c
  in
    (word, "string")
  end


fun pushInt(word : string) =
  let
    val a = Substring.full word
    val b = Substring.trimr 1 a
    val word = Substring.string b

    fun neg(word : string) =
      let
        val c = Substring.full word
        val d = Substring.triml 1 c
        val e = Substring.string d
        val list = ["~", e]
        val word = String.concat list
      in
        (word, "int")
      end

  in
    if isNeg(word) then neg(word)
    else (word, "int")
  end


fun push(word : string, stack : (string * string) list) =
  if isInt(word) then pushInt(word)::stack
  else
    if isString(word) then pushString(word)::stack
    else
      if isName(word) then pushName(word)::stack
      else
        if isBool(word) then pushBool(word)::stack
        else (":error:", "err")::stack


fun findSpace(line : char list) =
  if line = nil then 0
  else
    if hd(line) = #" " then 0
    else 1 + findSpace(tl(line))



fun split2(line : string) =
  let
    val a = Substring.full line
    val b = Substring.trimr 1 a
    val word = Substring.string b
  in
    (word, "")
  end


fun split(line : string) =
  if findSpace(explode(line)) = (String.size line)
  then split2(line)
  else
    let
      val a = Substring.full(line)
      val b = findSpace(explode(line))
      val c = Substring.splitAt(a, b)
      val d = Substring.string (#1 c)
      val e = Substring.string (#2 c)

      fun subsplit(line : string) = (d, String.extract(e, 1, NONE))
    in
      subsplit(line)
    end


fun compute(line : string, stack : (string * string) list, binder : (string * (string * string)) list) =
  let
    val operator = split(line)
    val op1 = (#1 operator)
    val op2 = (#2 operator)

    fun subpute(op1 : string) =
      case op1 of
          "push" => (push(op2, stack), binder)
        | "pop" => (pop(stack), binder)
        | "add" => (math(op1, stack, binder), binder)
        | "sub" => (math(op1, stack, binder), binder)
        | "mul" => (math(op1, stack, binder), binder)
        | "div" => (math(op1, stack, binder), binder)
        | "rem" => (math(op1, stack, binder), binder)
        | "neg" => (neg(stack, binder), binder)
        | "swap" => (swap(stack), binder)
        | "cat" => (cat(stack, binder), binder)
        | "and" => (bool(op1, stack, binder), binder)
        | "or" => (bool(op1, stack, binder), binder)
        | "not" => (not(stack, binder), binder)
        | "equal" => (math(op1, stack, binder), binder)
        | "lessThan" => (math(op1, stack, binder), binder)
        | "bind" => bind(stack, binder)
        | "if" => (ifop(stack, binder), binder)
        | "quit" => (quit(stack), binder)
        | _ => ((":error:", "err")::stack, binder)

  in
    subpute(op1)
  end


fun write(outStream : TextIO.outstream, stack : (string * string) list) =
  let

    fun neg(word : string) =
      let
        val a = Substring.full word
        val b = Substring.triml 1 a
        val c = Substring.string b
        val list = ["-", c]
        val word = String.concat list
      in
        word
      end

    fun call(head : string * string) =
      let
        val a = #1 head
        val b = #2 head
      in
        if b = "int" andalso revNeg(a)
        then neg(a)
        else a
      end


    fun subwrite(stack : (string * string) list) =
        case (List.length stack) of
          0 => TextIO.closeOut outStream
        | 1 => (TextIO.output(outStream, (call(hd(stack)))); TextIO.closeOut outStream)
        | _ => (TextIO.output(outStream, (call(hd(stack)))); TextIO.output(outStream, "\n"); subwrite(tl(stack)))
  in
    subwrite(stack)
  end


fun interpreter(inFile : string, outFile : string) =
  let
    val inStream = TextIO.openIn inFile
    val outStream = TextIO.openOut outFile
    val readLine = TextIO.inputLine inStream

    fun reader(readLine : string option, lists : (string * string) list * (string * (string * string)) list) =
	  let
	    val stack = #1 lists
	    val binder = #2 lists
	  in
	    case readLine of
	        NONE => (TextIO.closeIn inStream; write(outStream, stack))
	      | SOME(c) => (reader(TextIO.inputLine inStream, compute(c, stack, binder)))
      end

  in
    reader(readLine, ([], []))
  end


(* val _ = interpreter("input.txt", "output.txt") *)
