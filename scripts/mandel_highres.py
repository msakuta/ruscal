def printdensity(d):
  if d > 127:
    print(" ", end="")
  elif d > 8:
    print(".", end="")
  elif d > 4:
    print("+", end="")
  else:
    print("*", end="")

def mandelconverge(creal, cimag):
    r = creal
    i = cimag
    for iter in range(255):
        if r*r + i*i > 4:
            break
        next_r = r*r - i*i + creal
        i = 2*r*i + cimag
        r = next_r
    return iter

def mandel(xmin, ymin, xstep, ystep, xsteps, ysteps):
    xmax = xmin + xstep * xsteps
    ymax = ymin + ystep * ysteps
    print("xstep", xstep, "ysteps", ysteps, "ystep", ystep)
    for iy in range(ysteps):
        y = iy * (ymax - ymin) * ystep + ymin
        for ix in range(xsteps):
            x = ix * (xmax - xmin) * xstep + xmin
            printdensity(mandelconverge(x,y))
        print("")

mandel(-2.3, -2.0, 0.025 / 2, 0.05 / 2, 156, 80)
