//
//  BivariateNorm.h
//  rnnlib_xcode
//
//  Created by Sergey Zyrianov on 14/01/14.
//
//

#ifndef rnnlib_xcode_BivariateNorm_h
#define rnnlib_xcode_BivariateNorm_h
#include "Helpers.hpp"

namespace BivariateNorm {
real_t Z(real_t x, real_t y, real_t mu_x, real_t mu_y, real_t dev_x,
         real_t dev_y, real_t rho);
}

#endif
