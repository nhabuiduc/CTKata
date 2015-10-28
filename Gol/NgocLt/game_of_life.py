import copy

def tick(original, step):
    print 'Life after %d generations' % (step)
    for time in range(0, step):
        new_generation = copy.deepcopy(original)
        for y in range(len(original)):
            for x in range(len(original[0])):
                neighbours = live_neighbours(original, x, y)
                if (original[y][x] == 1) and (neighbours < 2 or neighbours > 3):
                    new_generation[y][x] = 0
                elif (original[y][x] == 0) and (neighbours == 3):
                    new_generation[y][x] = 1

        original = new_generation

    return original


def live_neighbours (generation, x, y):
    nor = len(generation)
    noc = len(generation[1])
    count = 0
    if x-1 >= 0:
        count += generation[y][x-1]

        if y-1 > 0:
            count += generation[y-1][x-1]
        if y+1 < nor:
            count += generation[y+1][x-1]

    if x+1 < noc:
        count += generation[y][x+1]

        if y-1 > 0:
            count += generation[y-1][x+1]
        if y+1 < nor:
            count += generation[y+1][x+1]

    if y-1 > 0:
        count += generation[y-1][x]

    if y+1 < nor:
        count += generation[y+1][x]

    return count

def print_lives(original):
    print '\n   ',
    for i in range(len(original[1])):
        print '%2d' % (i),
    print
    
    print '   ',
    for i in range(len(original[1])):
        print '__',
    print

    for i, row in enumerate(original):
        print '%2d |' % (i), '  '.join([str(cell) for cell in row])
    print

def main():
    game_of_live = [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0],
                    [0,1,1,1,0,0,0,0,0,1,0,0,1,1,0,0],
                    [0,0,0,0,0,0,0,0,1,1,0,0,1,0,1,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
                    [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

    print_lives(game_of_live)
    game_of_live = tick(game_of_live, 1)
    print_lives(game_of_live)

main()
