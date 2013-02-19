
## Output Files

#### out.info

File with infos about `out.csv`.

This simulation writes this fields:

* `time=` The total time of the simulation,
* `integral_step=` The time difference per step (not per line, because some results are deleted)
* `opt_fps=` The optimum framerate to have the simulation 1:1 with real time,
* `time_step= 1 / opt_fps` This is the the time difference per line in the CSV files,
* `frames_loss= 1 - optFps * timestep` Rate of frames lost due to deleting lines from result,
* `l1=` Length of first pendulum in meters,
* `l2=` Length of second pendulum in meters.

(all times in seconds).

The awk script `stats.awk` adds these 14 lines (numbers depending on out.csv):

```
t1min=8.893456163291235e-6
v1min=-39.239972520474666
t2min=-7307.913663851461
v2min=-53.9017307275152
tmin=1.6374351711813295e-2
vmin=-93.1348937853547
emin=-10.463647135986875
t1max=17965.00101238706
v1max=39.239955182574064
t2max=63442.190749256035
v2max=53.89714297265848
tmax=75913.92902903115
vmax=93.1350616460714
emax=75971.42898412446
```

These are the minimum and maximum values for the energies.

The last part is from the fourier transform:

* `fourier_window=` Length of fourier window in samples,
* `fourier_pgm_scaling=` How to scale the values in the pgm to retrieve the actual frequency amounts: `actual_val = pgm_val / pgm_scaling`

#### out.csv

Comma-separated values representing this:

`[phi1, phi2, p1, p2, kin1, pot1, kin2, pot2, (kin1+kin2), (pot1+pot2), (kin1+kin2+pot1+pot2)]`

Thats the content.

0. Angle of first pendulum,
0. Angle of second pendulum,
0. Momentum of first pendulum,
0. Momentum of second pendulum,
0. Kinetic energy of first pendulum,
0. Potential energy of first pendulum,
0. Kinetic energy of second pendulum,
0. Potential energy of second pendulum,
0. Sum of kinetic energies,
0. Sum of potential energies,
0. Sum of kinetic and potential energies.

(At all: 11 columns)
