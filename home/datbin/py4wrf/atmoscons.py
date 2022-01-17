def atmconstants():
# constants that are commonly used in atmospheric sciences. 
# Wanli Wu (wu80012@gmail.com)
# 09-14-2012
    atmcon = {'T0':273.15, 'G':9.80665, 'Rd':287.04, 'Rv':461.5, 'Cpd':1005.7, 'Cpv':1870, 'P0':1.0e5}
    epsilon = atmcon['Rd']/atmcon['Rv']
    kappa = atmcon['Rd']/atmcon['Cpd']
    onemore = {'epsilon':epsilon}
    atmcon.update(onemore)
    onemore = {'kappa':kappa}
    atmcon.update(onemore)

#   print epsilon, kappa
#   print atmcon

    return atmcon

