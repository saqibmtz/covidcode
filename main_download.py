# Import 
from helpers import extract,monthlyavg,febdaywise,social_distancing_metrics,related_brands,outside_inside
from compiling_master import compile_master
# Execute Function to compile files
extract()
monthlyavg()
febdaywise()
socialdistancingmetrics()
related_brands()
outside_inside()
compile_master()