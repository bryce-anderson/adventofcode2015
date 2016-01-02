from collections import deque


def parseInput(f):
    steps = []

    for l in f.readlines():
        l = l[:-1]
        parts = l.split()
        if len(parts) == 3:
            [a,_,c] = parts
            steps.append((c, a))

        elif len(parts) == 1:
            def o(s):
                c = 0
                for i in s[0]:
                    if i.isupper(): c += 1
                return -c

            return (l, steps) #sorted(steps, key = o))


def synthesis(formula, subs):
    def rep(formula):
        for (a,b) in subs:
            if a in formula:
                print('Substituting {} for {}'.format(b,a))
                return formula.replace(a,b,1)

        raise Exception('Failed to find substitute in {}'.format(formula))


    term = 'e'

    for i in xrange(len(formula)):
        if formula == term:
            print('Finished after {} steps'.format(i))
            return

        print('Iteration {}. formula size: {}'.format(i, len(formula)))
        formula = rep(formula)
        print formula


if __name__ == '__main__':
    filename = 'input'
    (formula,subs) = parseInput(open(filename))
    # print formula
    print subs
    # synthesis(formula, subs)
    s = open(filename).readlines()[-1][:-1]
    print s
    print s.replace('Rn','(').replace('Ar',')').replace('Y',',')

