import matplotlib.pyplot as plt



if __name__ == "__main__":

  x = [0, 200, 400,600, 800, 1000, 1020 ]
  y1 = [7.000000000000062E-006,  1.307900000000000E-002,  6.258200000000000E-002, 0.201683000000000 , 1.01908600000000,   1.61046600000000, 1.22791100000000]
  y2 = [5.999999999999929E-006, 4.725999999999998E-003, 5.851000000000001E-002, 0.201032000000000, 1.03330900000000,1.49718900000000, 1.20764700000000]
  y3 = [5.999999999999929E-006, 2.284999999999999E-003, 1.870400000000000E-002, 8.035500000000001E-002, 0.303610000000000, 0.646543000000000, 0.762052000000000]


  plt.plot(x, y1, 'r', label='EX1')
  plt.plot(x, y2, 'g', label='EX2')
  plt.plot(x, y3, 'b', label='MATMUL')
  plt.legend(loc='best')
  plt.yscale('log')
  plt.xscale('log')
  plt.title("TIME [s] (LOG SCALE)")
  plt.xlabel("N [NxN] (LOG SCALE)")
  plt.show()



#endmain
