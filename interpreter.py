stack = []
stacks = []
stacks2 = []
commands = []
binder = {}
binders = []
binders2 = []
functions = {}
functionList = []
functionList2 = []
count = 0


def interpreter(input, output):
    file_in = open(input, "r")
    file_out = open(output, "w")
    lines = file_in.readlines()
    global commands
    for line in lines:
        commands.append(line)
    compute()
    global stack
    stack.reverse()
    for val in stack:
        file_out.write(val[0])
        file_out.write("\n")


def compute():
    global commands
    global stack
    skip = 0
    counter = -1
    for line in commands:
        counter += 1
        sep = line.split()
        command = sep[0]

        if skip > 0:
            if command == 'funEnd':
                skip -= 1
            elif command == 'fun' or command == 'inOutFun':
                skip += 1
        elif command == 'push':
            sep.remove(command)
            action = line.replace(command + ' ', '')
            action = action.replace('\n', '')
            push(action)
        elif command == 'pop':
            pop()
        elif command == 'add' or command == 'sub' or command == 'mul' or command == 'div' or command == 'rem':
            math(command)
        elif command == 'neg':
            neg()
        elif command == 'swap':
            swap()
        elif command == 'cat':
            cat()
        elif command == 'and' or command == 'or':
            op_bool(command)
        elif command == 'not':
            not_bool()
        elif command == 'equal' or command == 'lessThan':
            compare(command)
        elif command == 'bind':
            bind()
        elif command == 'if':
            if_bool()
        elif command == 'let':
            let()
        elif command == 'end':
            end()
        elif command == 'fun' or command == 'inOutFun':
            sep.remove(command)
            if sep.__len__() == 2:
                name1 = sep[0]
                name2 = sep[1]
                fun(command, name1, name2, commands, counter)
                skip = 1
            else:
                pair = [":error:", "err"]
                stack.append(pair)
        elif command == 'call':
            call()
        elif command == 'quit':
            break
        elif command == 'return':
            return True


def push(action):
    global stack
    if is_int(action):
        if action == "-0":
            action = "0"
        pair = [action, "int"]
        stack.append(pair)
    elif action[0] == "\"" and action[action.__len__() - 1] == "\"":
        jackson = action.strip("\"")
        pair = [jackson, "string"]
        stack.append(pair)
    elif action[0] == "”" and action[action.__len__() - 1] == "”":
        jackson = action.strip("”")
        pair = [jackson, "string"]
        stack.append(pair)
    elif action[0] == ":":
        if action == ":true:" or action == ":false:":
            pair = [action, "bool"]
            stack.append(pair)
        else:
            pair = [":error:", "err"]
            stack.append(pair)
    elif is_name(action):
        pair = [action, "name"]
        stack.append(pair)
    else:
        pair = [":error:", "err"]
        stack.append(pair)


def pop():
    global stack
    try:
        stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)


def math(op):
    global stack
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "int" and pair2[1] == "int":
        if op == "add" or op == "sub" or op == "mul":
            if op == "add":
                val = int(pair1[0]) + int(pair2[0])
            elif op == "sub":
                val = int(pair1[0]) - int(pair2[0])
            else:
                val = int(pair1[0]) * int(pair2[0])
            pair = [str(val), "int"]
        else:
            if pair2[0] == "0":
                stack.append(pair1)
                stack.append(pair2)
                pair = [":error:", "err"]
            else:
                if op == "div":
                    val = int(pair1[0]) // int(pair2[0])
                if op == "rem":
                    val = int(pair1[0]) % int(pair2[0])
                pair = [str(val), "int"]
        stack.append(pair)
    else:
        comp = int_bind(pair1, pair2)
        if comp is not None:
            if op == "add" or op == "sub" or op == "mul":
                if op == "add":
                    val = int(comp[0]) + int(comp[1])
                elif op == "sub":
                    val = int(comp[0]) - int(comp[1])
                else:
                    val = int(comp[0]) * int(comp[1])
                pair = [str(val), "int"]
            else:
                if pair2[0] == "0":
                    stack.append(pair1)
                    stack.append(pair2)
                    pair = [":error:", "err"]
                else:
                    if op == "div":
                        val = int(comp[0]) // int(comp[1])
                    if op == "rem":
                        val = int(comp[0]) % int(comp[1])
                    pair = [str(val), "int"]
            stack.append(pair)
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)


def neg():
    global stack
    try:
        pair = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair[1] == "int":
        val = int(pair[0]) * -1
        pair = [str(val), "int"]
        stack.append(pair)
    else:
        if pair[0] in binder:
            comp = binder[pair[0]]
            if comp[1] == "int":
                val = int(comp[0]) * -1
                pair = [str(val), "int"]
                stack.append(pair)
            else:
                stack.append(pair)
                pair = [":error:", "err"]
                stack.append(pair)
        else:
            stack.append(pair)
            pair = [":error:", "err"]
            stack.append(pair)


def swap():
    global stack
    try:
        pair1 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        stack.append(pair1)
        return
    stack.append(pair1)
    stack.append(pair2)


def cat():
    global stack
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "string" and pair2[1] == "string":
        string = pair1[0] + pair2[0]
        pair = [string, "string"]
        stack.append(pair)
    else:
        comp = string_bind(pair1, pair2)
        if comp is not None:
            string = comp[0] + comp[1]
            pair = [string, "string"]
            stack.append(pair)
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)


def op_bool(op):
    global stack
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "bool" and pair2[1] == "bool":
        if op == "and":
            if pair1[0] == ":true:" and pair2[0] == ":true:":
                pair = [":true:", "bool"]
            else:
                pair = [":false:", "bool"]
        else:
            if pair1[0] == ":true:" or pair2[0] == ":true:":
                pair = [":true:", "bool"]
            else:
                pair = [":false:", "bool"]
        stack.append(pair)
    else:
        comp = bool_bind(pair1, pair2)
        if comp is not None:
            if op == "and":
                if comp[0] == ":true:" and comp[1] == ":true:":
                    pair = [":true:", "bool"]
                else:
                    pair = [":false:", "bool"]
            else:
                if comp[0] == ":true:" or comp[1] == ":true:":
                    pair = [":true:", "bool"]
                else:
                    pair = [":false:", "bool"]
            stack.append(pair)
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)


def and_bool():
    global stack
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "bool" and pair2[1] == "bool":
        if pair1[0] == ":true:" and pair2[0] == ":true:":
            pair = [":true:", "bool"]
            stack.append(pair)
        else:
            pair = [":false:", "bool"]
            stack.append(pair)
    else:
        comp = bool_bind(pair1, pair2)
        if comp is not None:
            if comp[0] == ":true:" and comp[1] == ":true:":
                pair = [":true:", "bool"]
                stack.append(pair)
            else:
                pair = [":false:", "bool"]
                stack.append(pair)
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)


def or_bool():
    global stack
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "bool" and pair2[1] == "bool":
        if pair1[0] == ":true:" or pair2[0] == ":true:":
            pair = [":true:", "bool"]
            stack.append(pair)
        else:
            pair = [":false:", "bool"]
            stack.append(pair)
    else:
        comp = bool_bind(pair1, pair2)
        if comp is not None:
            if comp[0] == ":true:" or comp[1] == ":true:":
                pair = [":true:", "bool"]
                stack.append(pair)
            else:
                pair = [":false:", "bool"]
                stack.append(pair)
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)


def not_bool():
    global stack
    try:
        pair = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair[1] == "bool":
        if pair[0] == ":true:":
            pair = [":false:", "bool"]
            stack.append(pair)
        else:
            pair = [":true:", "bool"]
            stack.append(pair)
    else:
        if pair[0] in binder:
            comp = binder[pair[0]]
            if comp[1] == "bool":
                if comp[0] == ":true:":
                    pair = [":false:", "bool"]
                    stack.append(pair)
                else:
                    pair = [":true:", "bool"]
                    stack.append(pair)
            else:
                stack.append(pair)
                pair = [":error:", "err"]
                stack.append(pair)
        else:
            stack.append(pair)
            pair = [":error:", "err"]
            stack.append(pair)


def compare(op):
    global stack
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "int" and pair2[1] == "int":
        if op == "equal":
            if int(pair1[0]) == int(pair2[0]):
                pair = [":true:", "bool"]
            else:
                pair = [":false:", "bool"]
            stack.append(pair)
        else:
            if int(pair1[0]) < int(pair2[0]):
                pair = [":true:", "bool"]
            else:
                pair = [":false:", "bool"]
            stack.append(pair)
    else:
        comp = int_bind(pair1, pair2)
        if comp is not None:
            if op == "equal":
                if int(comp[0]) == int(comp[1]):
                    pair = [":true:", "bool"]
                else:
                    pair = [":false:", "bool"]
                stack.append(pair)
            else:
                if int(comp[0]) < int(comp[1]):
                    pair = [":true:", "bool"]
                else:
                    pair = [":false:", "bool"]
                stack.append(pair)
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)
        

def bind():
    global stack
    global binder
    global functions
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "name" and can_bind(pair2[1]):
        if pair1[0] in binder:
            del binder[pair1[0]]
        binder[pair1[0]] = pair2
        pair = [":unit:", "unit", pair1[0]]
        stack.append(pair)
    elif pair1[1] == "name" and pair2[1] == "name":
        if pair2[0] in binder:
            binder[pair1[0]] = binder[pair2[0]]
            if pair2[0] in functions:
                functions[pair1[0]] = functions[pair2[0]]
            pair = [":unit:", "unit", pair1[0]]
            stack.append(pair)
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)
    else:
        stack.append(pair1)
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)


def if_bool():
    global stack
    try:
        pair3 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair2 = stack.pop()
    except IndexError:
        stack.append(pair3)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair3)
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "bool":
        if pair1[0] == ":true:":
            stack.append(pair2)
        else:
            stack.append(pair3)
    else:
        if pair1[0] in binder:
            comp = binder[pair1[0]]
            if comp[1] == "bool":
                if comp[0] == ":true:":
                    stack.append(pair2)
                else:
                    stack.append(pair3)
            else:
                stack.append(pair3)
                stack.append(pair2)
                stack.append(pair1)
                pair = [":error:", "err"]
                stack.append(pair)
        else:
            stack.append(pair3)
            stack.append(pair2)
            stack.append(pair1)
            pair = [":error:", "err"]
            stack.append(pair)


def let():
    global stack
    global stacks
    global binder
    global binders
    global functions
    global functionList
    global count
    if len(stack) == 0:
        stacks.append([])
    else:
        stacks.append(list(stack))
    if len(binder) == 0:
        binders.append({})
    else:
        binders.append(dict(binder))
    if len(functionList) == 0:
        functionList.append({})
    else:
        functionList.append(dict(functions))
    count += 1


def end():
    global stack
    global stacks
    global binder
    global binders
    global functions
    global functionList
    global count
    count -= 1
    try:
        try:
            val = stack.pop()
            stack = stacks[count]
            stack.append(val)
        except IndexError:
            stack = stacks[count]
        binder = binders[count]

        functions = functionList[count]
        stacks.pop()
        binders.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)


def fun(kind, name1, name2, command, start):
    global stack
    global binder
    global functions
    if is_name(name1) and is_name(name2):
        counter = 0
        local = []
        command = command[start:]
        skip = -1
        for line in command:
            if line[:4] == "fun " or line[:8] == "inOutFun":
                skip += 1
            if line == "funEnd\n":
                if skip == 0:
                    if len(stack) == 0:
                        fstack = []
                    else:
                        fstack = list(stack)
                    if len(binder) == 0:
                        fbinder = {}
                    else:
                        fbinder = dict(binder)
                    lookup = [name2, local, fstack, fbinder, kind]
                    functions[name1] = lookup
                    break
                else:
                    skip -= 1
                    local.append(line)
            elif counter > 0:
                local.append(line)
            counter += 1
        binder[name1] = ["fun"]
        pair = [":unit:", "unit"]
        stack.append(pair)
    else:
        pair = [":error:", "err"]
        stack.append(pair)
    return


def call():
    global stack
    global stacks2
    global binder
    global binders2
    global functions
    global functionList2
    global commands
    try:
        pair2 = stack.pop()
    except IndexError:
        pair = [":error:", "err"]
        stack.append(pair)
        return
    try:
        pair1 = stack.pop()
    except IndexError:
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)
        return
    if pair1[1] == "name" and is_valid(pair2):
        if pair1[0] in functions:
            lookup = functions[pair1[0]]
            if pair2[1] == "name":
                name = binder[pair2[0]]
            else:
                name = pair2
            backup = commands
            stacks2.append(stack)
            binders2.append(binder)
            functionList2.append(dict(functions))
            commands = lookup[1]
            stack = list(lookup[2])
            binder = dict(lookup[3])
            kind = lookup[4]
            binder[lookup[0]] = name
            if pair2[0] in functions:
                functions[lookup[0]] = functions[pair2[0]]
            check = compute()
            last = stack.pop()
            stack = stacks2.pop()
            checkmate = False
            if check:
                if is_name(last):
                    val = binder[last[0]]
                    if val[0] != "fun":
                        last = val
                        functions = functionList2.pop()
                    else:
                        val = functions[last[0]]
                        functions = functionList2.pop()
                        functions[last[0]] = val
                        checkmate = True
                else:
                    functions = functionList2.pop()
                stack.append(last)
            else:
                functions = functionList2.pop()
            if kind == "inOutFun":
                rebind = binder[lookup[0]]
                binder = binders2.pop()
                del binder[pair2[0]]
                binder[pair2[0]] = rebind
            else:
                binder = binders2.pop()
                if checkmate:
                    binder[last[0]] = ["fun"]
            commands = backup
        else:
            stack.append(pair1)
            stack.append(pair2)
            pair = [":error:", "err"]
            stack.append(pair)
    else:
        stack.append(pair1)
        stack.append(pair2)
        pair = [":error:", "err"]
        stack.append(pair)


def is_int(word):
    try:
        int(word)
        return True
    except ValueError:
        return False


def has_int(word):
    return any(char.isdigit() for char in word)


def is_name(word):
    if word[0].isalpha():
        for c in word:
            if c.isalnum():
                check = True
            else:
                check = False
                break
        return check
    else:
        return False


def is_valid(word):
    if word[0] == "fun" or word[1] == "int" or word[1] == "string" or word[1] == "bool" or word[1] == "unit":
        return True
    elif word[1] == "name":
        if word[0] in binder:
            new = binder[word[0]]
            return is_valid(new)
        else:
            return False
    else:
        return False


def can_bind(word):
    if word == "int" or word == "bool" or word == "string" or word == "unit":
        return True
    else:
        return False


def int_bind(pair1, pair2):
    global binder
    if pair1[1] == "name" and pair2[1] == "int":
        if pair1[0] in binder:
            val = binder[pair1[0]]
            if val[1] == "int":
                return [val[0], pair2[0]]
            else:
                return None
        else:
            return None
    elif pair1[1] == "int" and pair2[1] == "name":
        if pair2[0] in binder:
            val = binder[pair2[0]]
            if val[1] == "int":
                return [pair1[0], val[0]]
            else:
                return None
        else:
            return None
    elif pair1[1] == "name" and pair2[1] == "name":
        if pair1[0] in binder and pair2[0] in binder:
            val1 = binder[pair1[0]]
            val2 = binder[pair2[0]]
            if val1[1] == "int" and val2[1] == "int":
                return [val1[0], val2[0]]
            else:
                return None
        else:
            return None
    else:
        return None


def bool_bind(pair1, pair2):
    global binder
    if pair1[1] == "name" and pair2[1] == "bool":
        if pair1[0] in binder:
            val = binder[pair1[0]]
            if val[1] == "bool":
                return [val[0], pair2[0]]
            else:
                return None
        else:
            return None
    elif pair1[1] == "bool" and pair2[1] == "name":
        if pair2[0] in binder:
            val = binder[pair2[0]]
            if val[1] == "bool":
                return [pair1[0], val[0]]
            else:
                return None
        else:
            return None
    elif pair1[1] == "name" and pair2[1] == "name":
        if pair1[0] in binder and pair2[0] in binder:
            val1 = binder[pair1[0]]
            val2 = binder[pair2[0]]
            if val1[1] == "bool" and val2[1] == "bool":
                return [val1[0], val2[0]]
            else:
                return None
        else:
            return None
    else:
        return None


def string_bind(pair1, pair2):
    if pair1[1] == "name" and pair2[1] == "string":
        if pair1[0] in binder:
            val = binder[pair1[0]]
            if val[1] == "string":
                return [val[0], pair2[0]]
            else:
                return None
        else:
            return None
    elif pair1[1] == "string" and pair2[1] == "name":
        if pair2[0] in binder:
            val = binder[pair2[0]]
            if val[1] == "string":
                return [pair1[0], val[0]]
            else:
                return None
        else:
            return None
    elif pair1[1] == "name" and pair2[1] == "name":
        if pair1[0] in binder and pair2[0] in binder:
            val1 = binder[pair1[0]]
            val2 = binder[pair2[0]]
            if val1[1] == "string" and val2[1] == "string":
                return [val1[0], val2[0]]
            else:
                return None
        else:
            return None
    else:
        return None


#interpreter("p3/input/input.txt", "output.txt")