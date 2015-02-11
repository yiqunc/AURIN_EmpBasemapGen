# AURIN_EmpBasemapGen
Historical Employment Basemap Generator

PART1 - Installation and configuration
1. The provided R scripts are tested in R (X64 3.1.1), RStudio (0.98.1056) on Windows 7 Enterprise (64bit), 
2. Users need to change the "working directory" to their own data directory
3. The data sets for running the scripts are specifically designed and prepared and NOT included in this repo due to the giant file size. Please contact Dr Benny Chen (yiqun.c@unimelb.edu.au) and Dr Jennifer Day (jday@unimelb.edu.au) for more information.


PART2 - How to prepare basemap (example using 2006 VIC DZN and Planning Zone data sets)

1. load orignal DZN and Planning zone(PLZ) layer in ArcGIS: "DZN_2006_VIC","plan_zone_original_vic_2006", make sure they are in WGS84
2. intersect PLZ with DZN to create split polygons: "plan_zone_fullcode_split_vic_2006"
3. create inside centroid for split polygons: "plan_zone_fullcode_split_centroid_vic_2006"
4. (optional: if DZN_X_Employment_2006_Orginal exists, skip this step) merge DZN polygon with JTW data using "Emp_BasemapBuilder_(ostype).r" script:  "f_datamerge()", the output will be "DZN_X_Employment_2006_Orginal"
5. Adjust and Run "f_GenBaseMap_VIC_2006()"
6. Done


PART3 - How to prepare basemap (example using 2011 VIC DZN and Meshblock data sets)

1. load orignal DZN and Meshblock(MB) layer in ArcGIS: "DZN_2011_VIC","MB_2011_VIC", make sure they are in WGS84
2. intersect PLZ with DZN to create split polygons: "mb_fullcode_split_vic_2011" (ArcTookbox ->Analysis Tools->Overlay->Intersect)
3. create inside centroid for split polygons: "mb_fullcode_split_centroid_vic_2011" (ArcTookbox ->Data Management Tools->Features->Feature to Point)
4. (optional: if DZN_X_Employment_2011_Orginal exists, skip this step) merge DZN polygon with JTW data using "Emp_BasemapBuilder_(ostype).r" script:  "f_datamerge_2011()", the output will be "DZN_X_Employment_2011_Orginal"
5. Adjust and Run "f_GenBaseMap_VIC_2011_MeshBlock()"
6. Done

