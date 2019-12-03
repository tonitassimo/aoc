module Day01
    ( calculateTotalFuel,
      modules
    ) where

type Module = Int
type Fuel = Int
type Modules = [Module]


modules :: Modules
modules = [115175,57676,60193,72564,80321,71598,105010,
           117412,131402,123335,115916,149847,101974,
           106640,60426,79110,96761,113017,61374,115030,
           93834,70762,65579,141554,72032,79904,90575,
           103557,133428,59508,118219,138231,103913,
           103836,54845,110272,121529,71820,104270,78622,
           117267,74147,145980,118616,69569,140008,89901,
           97815,67603,133165,100395,96554,53074,88629,
           74968,129337,62372,77034,102219,53661,54411,
           95923,53990,105827,61721,84050,128613,125007,
           127985,56573,116470,77464,112006,142367,111543,
           71770,121529,147762,119612,126423,148684,78702,
           86829,65985,78223,81857,83423,147118,129117,147612,
           63482,57350,126132,88534,90676,56669,120383,126892,74203,103766]

calculateTotalFuel :: Modules -> Fuel
calculateTotalFuel [] = 0 
calculateTotalFuel (x:xs) = let fuelForModule = calculateFuelForModule x in
                                (fuelForModule + calculateFuelForFuel fuelForModule) + calculateTotalFuel xs

calculateFuelForModule :: Module -> Fuel
calculateFuelForModule = baseCalculation 

calculateFuelForFuel :: Fuel -> Fuel
calculateFuelForFuel f = let res = baseCalculation f in
                    if res < 0 
                        then 0
                        else res + calculateFuelForFuel res

baseCalculation :: Int -> Int
baseCalculation i = (div i 3) - 2