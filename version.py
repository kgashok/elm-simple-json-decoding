#!/usr/bin/env python

# Inspired by 
# https://gitfu.wordpress.com/2008/05/25/git-describe-great-another-way-to-refer-to-commits/
# http://gitready.com/beginner/2009/02/03/tagging.html


versionFile = "Version.elm"
defaultRepo = "http://github.com/kgashok/elm-simple-json-decoding" 


#####################
## Get Version details
######################
import commands

status, repo = commands.getstatusoutput ("git ls-remote --get-url") 
if status: 
	repo = defaultRepo 
else:
	#repo = repo.split (".git")[0] 
	import re 
	repo = re.sub('\.git$', '', repo) 

status, version = commands.getstatusoutput ("git describe --tags --long")
if not status: 
	print ("Version: " + version)
else: 
	print "git describe returned bad status!"
	print "The repo should have at least one release tag!"
	print "Please see https://help.github.com/articles/creating-releases/"
	version = "NA"

previous = None
try:
	fo = open (versionFile, "r")
	#print "Name of file opened ", fo.name 
	previous = fo.read()
	#print (previous)
	fo.close()
	
except:
	if version != "NA":
		print ("Creating new version file...")


######
# Is it really necessary to update? 
######
if version == "NA":
	print (versionFile + ": generation of file aborted!") 
elif previous and previous.find (version) != -1:
	print (versionFile + " already up-to-date!")
else: 
	####################
	## build elm code 
	####################
	fileContent = \
	'''module Version exposing (..) \n\
{-| This file is auto-generated by the Python script version.py.\n
-} \n\
\n\
\n\
version : String \n\
'''

	fileContent = fileContent + 'version = "' + version + '"\n\n'
	fileContent = fileContent + 'gitRepo : String \n'
	fileContent = fileContent + 'gitRepo = "' + repo + '"\n\n'
	##################
	## Just to be safe, we must backing up previous versionFile
	##################
	pass  # Not yet implemented 

	fo = open (versionFile, "w+")
	fo.write (fileContent)
	#print (fileContent)
	print (versionFile + " updated with " + version)
	fo.close()

commands.getstatusoutput ("elm-format Version.elm --yes") 
commands.getstatusoutput ("git add Version.elm")
commands.getstatusoutput ("elm make FreeCodeCamp.elm --output dist/elm.js")
print(commands.getoutput("grep v3.5 dist/elm.js"))
