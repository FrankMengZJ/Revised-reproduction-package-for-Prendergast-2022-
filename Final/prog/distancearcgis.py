# -*- coding: utf-8 -*-
"""
Created on Wed Jul 31 14:27:35 2019

get_distances.py

Automate trucking distances from warehouse origins to choicemember locations using ArcGIS

@author: Wendy Wong
"""

import arcgis
from arcgis.gis import GIS
import pandas as pd
import datetime
import getpass
from IPython.display import HTML
import json

from arcgis import geocoding
from arcgis.features import Feature, FeatureSet
from arcgis.features import GeoAccessor, GeoSeriesAccessor
from arcgis.network.analysis import generate_origin_destination_cost_matrix


inputPath = './data/'
outputPath = './data/'
#os.chdir(outputPath)

portal_url = "https://uchicago.maps.arcgis.com"
mytoken = "I9Bbj1cCNV2BJ9VTivm-07N1GbKNLFRO2eHHl8LT2ixS5qEphFMyYp7T1SlO3TSCvyjRSbyh_CvC8hFub7VQ9H1x4vLdgBlzGI5mzrmPWOvDfK3R4dqHXGNGdAW9ampWfnDYXCGbunoisvNOVtTTqlNJd4x7XPBnD7yy9VWlKQ8RdLZEpVS918zX1XcCpFPUkMoPb2ZLpckPytyc0StDw7CjRNfF_y1yREbosULHG3I."
gis = GIS(url=portal_url,token=mytoken)

#load origins data
origin = pd.read_csv(inputPath+'locwarehouses.csv',dtype={'ID':object,'CITY':object,'STATE':object,'ZIP':object,'COUNTRYCODE':object,'Longitude':object,'Latitude':object,'Match Score':object})
origin['ADDRESS'] = origin.CITY.fillna('') + ', ' + origin.STATE.fillna('') + ', ' + origin.ZIP.fillna('') + ', ' + origin.COUNTRYCODE.fillna('')
origin = origin.sort_values(by = 'ID')
origin_dict = pd.DataFrame(origin, columns=['ID','ADDRESS','Latitude','Longitude']).to_dict(orient='records')
destination = pd.read_csv(inputPath+'locchoicemembers.csv',dtype={'ID':object,'ADDRESS':object,'CITY':object,'STATE':object,'ZIP':object,'COUNTRYCODE':object,'Longitude':object,'Latitude':object,'Match Score':object})
destination['ADDRESS_full'] = destination.ADDRESS.fillna('') + ', ' + destination.CITY.fillna('') + ', ' + destination.STATE.fillna('') + ', ' + destination.ZIP.fillna('') + ', ' + destination.COUNTRYCODE.fillna('')
destination = destination.sort_values(by = 'ID')
destination_dict = pd.DataFrame(destination, columns=['ID','ADDRESS_full','Latitude','Longitude']).to_dict(orient='records')

#create origin/destination layer for input into OD calculator

#test1 = origin_dict[5:6]
#set1: 0:1000
#set2: 1001:2000
#set3: 2001:2515
origin_features = []
for i in origin_dict[2001:2515]:
    origin_feature = Feature(geometry = {"x": i['Longitude'],
                                         "y": i['Latitude']},
                            attributes = {"ID": i['ID'],
                                          "Address": i['ADDRESS']})
    origin_features.append(origin_feature)

origin_fset = FeatureSet(origin_features, geometry_type = 'esriGeometryPoint', spatial_reference = {'latestWkid':4326})

#test2 = destination_dict[1:2]
destination_features = []
for i in destination_dict:
    destination_feature = Feature(geometry = {"x": i['Longitude'],
                                             "y": i['Latitude']},
                                attributes = {"ID": i['ID'],
                                              "Address": i['ADDRESS_full']})
    destination_features.append(destination_feature)

destination_fset = FeatureSet(destination_features, geometry_type = 'esriGeometryPoint', spatial_reference = {'latestWkid':4326})

#input layers into OD calculator
results = generate_origin_destination_cost_matrix(origins= origin_fset, #origins_fc_latlong, 
                                                destinations= destination_fset, #destinations_fs_address,
                                                #cutoff=200,
                                                restrictions=['Driving a Truck'],
                                                origin_destination_line_shape='Straight Line')
print('Analysis succeeded? {}'.format(results.solve_succeeded))

#process and save results
od_df = results.output_origin_destination_lines.sdf
od_df['OrigLat'] = [eval(x)['paths'][0][0][1] for x in od_df.SHAPE.tolist()]
od_df['OrigLong'] = [eval(x)['paths'][0][0][0] for x in od_df.SHAPE.tolist()]
od_df['DestLat'] = [eval(x)['paths'][0][1][1] for x in od_df.SHAPE.tolist()]
od_df['DestLong'] = [eval(x)['paths'][0][1][0] for x in od_df.SHAPE.tolist()]

od_df.to_csv(path_or_buf = outputPath+'/warehousecitycentroid_choicemem_set3.csv', index = False)