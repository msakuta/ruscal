function printdensity(d) {
  if (d > 127) {
    process.stdout.write(" ")
  } else if (d > 8) {
    process.stdout.write(".");
  } else if (d > 4) {
    process.stdout.write("+");
  } else {
    process.stdout.write("*")
  }
}

function mandelconverge(creal, cimag) {
    var r = creal;
    var i = cimag;
    for (var iter = 0; iter < 255; iter++) {
        if (r*r + i*i > 4) {
            break;
        };
        var next_r = r*r - i*i + creal;
        i = 2*r*i + cimag;
        r = next_r;
    }
    return iter;
}

function mandel(xmin, ymin, xstep, ystep, xsteps, ysteps) {
    var xmax = xmin + xstep * xsteps;
    var ymax = ymin + ystep * ysteps;
    console.log("xstep", xstep, "ysteps", ysteps, "ystep", ystep);
    for (var iy = 0; iy < ysteps; iy++) {
        var y = iy * (ymax - ymin) * ystep + ymin;
        for (var ix = 0; ix < xsteps; ix++) {
            var x = ix * (xmax - xmin) * xstep + xmin;
            printdensity(mandelconverge(x,y));
        }
        process.stdout.write("\n");
    }
}

mandel(-2.3, -2.0, 0.025 / 2, 0.05 / 2, 156, 80);
