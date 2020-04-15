#===============================================================================
# Copyright UCAR (c) 2008-2013.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Laboratory (RAL),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Terri Betancourt, Jan 2014
#
# Purpose: To generate a metadata documentation based on input from
#          GCAT's xml configurations file contents
#
# Usage:
#   mk_gcat_metadata.py --config /d1/GCAT/wrf/server/job_config/GCW0051
#   (job type CLIMO, output to stdout)
#
#   mk_gcat_metadata.py --config /d1/GCAT/wrf/server/job_config/SOM0069 --output README.metadata
#   (job type CLIMO+SOM, output to file)
#
#   mk_gcat_metadata.py --config /d1/GCAT/wrf/server/job_config/SOM0069 --parent /mydir/climo.config.xml
#   (job type CLIMO+SOM, where parent config file does not live in default sibling directory)
#
# Supported:
#   The following job types are supported: climo, som, case, forecast
#
#===============================================================================
import sys
import os, traceback
import argparse
import re
from xml.etree import ElementTree as et

      
class jobConfig:    
    #
    # Class for parsing a generic job_configuration.xml file to get the job type
    #
    def __init__(job,configXML=None):
        job.type = None
        job.root = None

        if configXML is not None:
            job.parseConfigFile(configXML)

    def parseConfigFile(job,configXML):
        job.root = et.parse(configXML).getroot()
        for child in job.root:
            tag = child.tag
            if tag == 'Type':
                job.type = child.text

class somConfig:    
    #
    # Class for parsing a SOM job_configuration.xml file
    #
    def __init__(som,jobRoot,baseXML = None):
        som.baseXML = baseXML
        som.climoRoot = None
        som.parentJobID = None
        som.parentJobGMID = None
        som.description = None
        som.jobID = None
        som.GMID = None
        som.user = None
        som.platform = None
        som.outputURL = None
        som.domain = None
        som.classifiers = []
        som.classifierLevel = []
        som.length = None
        som.startingHour = None
        som.frequency = None

        if jobRoot is not None:
            som.parseConfigRoot(jobRoot)

    def parseConfigRoot(som,jobRoot):
        for child in jobRoot:
            tag = child.tag
            if tag == 'ParentJob_ID':
                som.parentJobID = child.text
            elif tag == 'ParentJob_GMID':
                som.parentJobGMID = child.text
            elif tag == 'Description':
                som.description = child.text
            elif tag == 'GMID':
                som.GMID = child.text
            elif tag == 'Job_ID':
                som.jobID = child.text
            elif tag == 'User':
                som.user = child.text
            elif tag == 'Platform':
                som.platform = child.text
            elif tag == 'OutputURL':
                som.outputURL = child.text
            elif tag == 'JobConfig':
                for element in child:
                    tag = element.tag
                    if tag == 'Domain':
                        som.domain = element.text.strip()[-1]
                    elif tag == 'Classifiers':
                        for classifier in element.iter('Classifier'):
                            if classifier.text == 'surf_uv':
                                som.classifiers.append('U10')
                                som.classifierLevel.append(-1)
                                som.classifiers.append('V10')
                                som.classifierLevel.append(-1)
                            elif classifier.text == 'pbl_uv':
                                som.classifiers.append('U')
                                som.classifierLevel.append(10)
                                som.classifiers.append('V')
                                som.classifierLevel.append(10)
                            elif classifier.text == 'pbl_h':
                                som.classifiers.append('PBLH')
                                som.classifierLevel.append(-1)
                            elif classifier.text == 'surf_dosage':
                                som.classifiers.append('DOSAGE')
                                som.classifierLevel.append(-1)
                            else:
                                print 'WARNING: Unrecognized SOM classifier in configuration file'
                    elif tag == 'Length':
                        som.length = int(element.text)
                    elif tag == 'Starting_hour':
                        som.startingHour = int(element.text)
                    elif tag == 'Frequency':
                        som.frequency = int(element.text)
        
    def getClimoRoot(som,parentXML):
        if parentXML is None:
            climoConfigPath = None
            try:
                #
                # Derive the parent XML config file if none was passed in on the command line
                #
                climoConfigPath = os.path.join('..', som.parentJobGMID, 'job_configuration.xml')
                if not os.path.exists(climoConfigPath) and None != som.baseXML:
                    tmpClimoConfigPath = os.path.join(os.path.dirname(som.baseXML.name),climoConfigPath)
                    if os.path.exists(tmpClimoConfigPath):
                        climoConfigPath = tmpClimoConfigPath
                
                parentXML = open(climoConfigPath,'r')
            except:
                print 'ERROR: Unable to open parent XML file: ', climoConfigPath
                traceback.print_exc(file=sys.stdout)
                sys.exit(1)
        #
        # Fetch the root of the xml tree for the parent climo job
        #
        som.climoRoot = et.parse(parentXML).getroot()


    def writeMetadata(som):
        #
        # Print out subset of xml configuration to metadata keyword pairs
        #
        cmdArgs.output.write( 'SOMid = {0} ({1})\n'.format(som.jobID,som.GMID) )
        cmdArgs.output.write( 'SOMname = \'{0}\'\n'.format(som.description) )
        cmdArgs.output.write( 'SOMclassifiers = {0}\n'.format(str(som.classifiers).strip('[]')) )
        cmdArgs.output.write( 'SOMfrequency = {0}\n'.format(som.frequency) )
        cmdArgs.output.write( 'SOMlength = {0}\n'.format(som.length) )
        cmdArgs.output.write( 'SOMstartHour = {0}\n'.format(som.startingHour) )
        cmdArgs.output.write( 'SOMdomain = {0}\n'.format(som.domain) )

class climoConfig:
    #
    # Class for parsing a CLIMO job_configuration.xml file
    #
    def __init__(climo,jobRoot):
        climo.user = None
        climo.platform = None
        climo.description = None
        climo.GMID = None
        climo.outputURL = None

        climo.runType = None
        climo.modelType = None
        climo.latitude = None
        climo.longitude = None
        climo.obsYears = None
        climo.startTime = None
        climo.endTime = None
        climo.preset = None
        climo.customCrossSections = None
        climo.customFDDASites = None

        if jobRoot is not None:
            climo.parseConfigRoot(jobRoot)

    def parseConfigRoot(climo,jobRoot):
        for child in jobRoot:
            tag = child.tag
            if tag == 'User':
                climo.user = child.text
            elif tag == 'Platform':
                climo.platform = child.text
            elif tag == 'Description':
                climo.description = child.text
            elif tag == 'GMID':
                climo.GMID = child.text
            elif tag == 'OutputURL':
                climo.outputURL = child.text
            elif tag == 'JobConfig':
                for element in child:
                    tag = element.tag
                    if tag == 'RunType':
                        climo.runType = element.text
                    elif tag == 'ModelType':
                        climo.modelType = element.text
                    elif tag == 'Latitude':
                        climo.latitude = element.text
                    elif tag == 'Longitude':
                        climo.longitude = element.text
                    elif tag == 'ObsYears':
                        climo.obsYears = element.text
                    elif tag == 'StartTime':
                        climo.startTime = element.text
                    elif tag == 'EndTime':
                        climo.endTime = element.text
                    elif tag == 'Preset':
                        climo.preset = element.text
                    elif tag == 'CustomCrossSections':
                        climo.customCrossSections = element.text
                    elif tag == 'CustomFDDASites':
                        climo.customFDDASites = element.text

    def writeMetadata(climo):
        #
        # Extract the climo job id from the outputURL
        #
        m = re.match('.*jobIdW_(.*)$', climo.outputURL)
        jobIdW = m.group(1)

        #
        # Print out subset of xml configuration to metadata keyword pairs
        #
        cmdArgs.output.write( 'CFDDAid = {0} ({1})\n'.format(jobIdW,climo.GMID) )
        cmdArgs.output.write( 'CFDDAname = \'{0}\'\n'.format(climo.description) )
        cmdArgs.output.write( 'CFDDAstartMonth = {0}\n'.format(climo.startTime[0:2]) )
        cmdArgs.output.write( 'CFDDAstartDay = {0}\n'.format(climo.startTime[2:4]) )
        cmdArgs.output.write( 'CFDDAstartHour = {0}\n'.format(climo.startTime[4:6]) )
        cmdArgs.output.write( 'CFDDAendMonth = {0}\n'.format(climo.endTime[0:2]) )
        cmdArgs.output.write( 'CFDDAendDay = {0}\n'.format(climo.endTime[2:4]) )
        cmdArgs.output.write( 'CFDDAendHour = {0}\n'.format(climo.endTime[4:6]) )
        cmdArgs.output.write( 'CFDDAyears = {0}\n'.format(climo.obsYears) )
        cmdArgs.output.write( 'CFDDAlatitude = {0}\n'.format(climo.latitude) )
        cmdArgs.output.write( 'CFDDAlongitude = {0}\n'.format(climo.longitude) )
        cmdArgs.output.write( 'CFDDAinnerDomain = {0}\n'.format(climo.preset[-1]) )

#
# Handle command line input
#
def processCommandLine():
    parser = argparse.ArgumentParser(description='Generate metadata from GCAT XML configuration file(s)')
    parser.add_argument("-c", "--config", required=True, type=argparse.FileType('r'), help="XML configuration file (Climo or SOM)")
    parser.add_argument("-p", "--parent", required=False, type=argparse.FileType('r'), help="Parent XML configuration file (Climo)")
    parser.add_argument("-o", "--output", required=False, type=argparse.FileType('w'), default=sys.stdout, help="Output metadata file")
    return parser.parse_args()

            
#
# Main entry point
#
if __name__ == "__main__":

        #
        # Process command line and see if we should open an output file
        #
        cmdArgs = processCommandLine()

        #
        # Fetch and process the job config file specified on the command line
        #
        jConfig = jobConfig(cmdArgs.config)
        
        #               
        # Process the bulk of xml file based on type tag
        #               
        cmdArgs.output.write(  'Type = {0}\n'.format(jConfig.type) )
        if jConfig.type == 'som':
            #           
            # Type==SOM implies two xml files to process (som and cfdda)
            # First process the SOM config file
            #
            sConfig = somConfig( jConfig.root, cmdArgs.config )
            sConfig.writeMetadata()

            #
            # Fetch and process the SOM's associated Climo job config file
            #
            sConfig.getClimoRoot(cmdArgs.parent)
            cConfig = climoConfig( sConfig.climoRoot )

            #
            # Insure that the climo configuration matches the SOM parentGMID
            #
            if cConfig.GMID != sConfig.parentJobGMID:
                print 'ERROR: Mismatch between --parent (', cConfig.GMID, ') and --config (', sConfig.parentJobGMID, ') job IDs'
                sys.exit(1)

            cConfig.writeMetadata()

        elif jConfig.type == 'climo' or jConfig.type == 'forecast' or jConfig.type == 'case':
            #
            # Type==Climo implies only one xml file to process (cfdda)
            #
            cConfig = climoConfig( jConfig.root )
            cConfig.writeMetadata()

        else:
            print 'ERROR: unrecognized <Type> value in xml configuration file:', jConfig.type
            sys.exit(1)

