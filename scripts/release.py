#! /usr/bin/env python3

# This script peforms the following:
# - Update the product verison in the build.sbt file
# - Update the download link in the README.md file
# - Commit changes to the git repo
# - Create a release tag
# - Push commits and tag to Github

import sys, subprocess, re, os
import argparse

class AbortException(Exception):
  def __init__(self, msg):
    super().__init__(msg)

class CmdFailedException(AbortException):
  def __init__(self, result):
    super().__init__(msg = f"Command did not exit successfully: {result.args}")
    self.result = result
  
def abort(msg):
  raise AbortException(msg)

# cmd - a string with the command arguments
# echo - print the command to stdout before running the command
# pipe - use a pipe to caputre stdout and stderr
def capture_output(cmd):
  result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  if result.returncode != 0:
    raise CmdFailedException(result)
  return result.stdout.decode('utf-8')

# cmd - a string with the command arguments
# echo - print the command to stdout before running the command
# pipe - use a pipe to caputre stdout and stderr
def system_command(cmd, echo = False):
  if echo:
    print(" ".join(cmd))
  result = subprocess.run(cmd)
  if result.returncode != 0:
    raise CmdFailedException(result)
  return result

def system_commands(cmds, echo = False):
  for cmd in cmds:
     system_command(cmd, echo = echo)

# Return True if the working directory is dirty
def working_directory_dirty():
  return capture_output(['git', 'status', '--porcelain']).strip() != ''

def getYorN(prompt):
  while True:
    response = input(f"\n{prompt} (y/n) ").lower().strip()
    if re.search(r'^y.*', response):
       return True
    elif re.search(r'^n.*', response):
       return False
    else:
       print("Invalid response")

# extract the current version number from build.sbt
# Requires the current working directory to be the top level git repo dir
def get_current_version():
  with open('build.sbt') as f:
    contents = f.read()
    match re.search(r'^\s*version\s*:=\s*"([^"]+)"', contents, re.MULTILINE):
      case None:
        abort("Cannot determine current version!")
      case match:
        return match[1]
    

# update the current version number in build.sbt
def set_version(version):
  with open('build.sbt', 'r+') as f:
     contents = f.read()
     contents = re.sub(r'(version\s*:=\s*)"\d+.\d+"', f'\\1"{version}"', contents)
     f.seek(0)
     f.write(contents)
     f.truncate()
  print(f'Version set to {version}')

# name of zipfile base on given version
def zipfile_name(version):
   return f'{program_name}-{version}.zip'

# Add changes that we have made to the git index and commit them
# Create a tag for the given version
# Push the commit and tag to Github
# Create a Github release for the version
def commit_release(version):
  version_label = f'v{version}'
  local_zip_file_path = f'target/{zipfile_name(version)}'
  cmds = [
    ['git', 'add', '--update', '.'],
    ['git', 'commit', f'-mbuild: update version number to {version}'],
    ['git', 'tag', f'-mRelease {version_label}', version_label],
    ['git', 'push', '--tags', 'origin', 'master'],
    # Create the release and upload the zip file to the release assests
    ['gh', 'release', 'create', '--generate-notes', '--title', f'Version {version}', version_label, local_zip_file_path]
  ]
  system_commands(cmds, echo = True)


# Update the README.md file with the new version number and dropbox url
def update_readme(version):
  zip_file_url = f'https://github.com/sellmerfud/{repo_name}/releases/download/v{version}/{zipfile_name(version)}'
  with open('README.md', 'r+') as f:
     contents = f.read()
     contents = re.sub(r'\[Version\s*\d+\.\d+\]', f'[Version {version}]', contents)
     contents = re.sub(r'^\[1\]:.*$', f'[1]: {zip_file_url}', contents, flags = re.MULTILINE)
     f.seek(0)
     f.write(contents)
     f.truncate()

# usage: release.py [--commit|--no_commit] [VERSION]
#   --commit    - Commit changes and push them to Github (Default)
#   --no-commit - Do not commit changes

#   VERSION     - Can be one of:
#                 next_minor: Bump the minor version number (Default)
#                 next_major: Bump the major version number and set minor to zero
#                 <major>.<minor>: where: major and minor are integers

version_help =\
'''next_minor      - Bump the minor version number
next_major      - Bump the major version number and set minor to zero
<major>.<minor> - where major and minor are integers
If omitted it defaults to next_minor
'''
parser = argparse.ArgumentParser(formatter_class=argparse.RawTextHelpFormatter)
parser.add_argument('--commit', action=argparse.BooleanOptionalAction, default=True, help='Commit changes and push them to Github (Default=true)')
parser.add_argument('version', metavar='VERSION', type=str,  nargs='?', default='next_minor', help=version_help)

# Main entry point of script
# Program name and dropbox folder are used to
# upload the zip file to dropbox
repo_name='awakening'
program_name='awakening'

try:
  args = parser.parse_args()

  if re.search(r'\A\d+\.\d+\z', args.version) != None:
    new_version= args.version
  elif args.version == '' or args.version.lower() == 'next_minor':
      new_version='next_minor'  # Default value
  elif args.version.lower() == 'next_major':
      new_version='next_major'
  else:
    abort(f'Invalid version argument: {args.version}')

  ## Set the current working directory to the parent directory of this script.
  ## (The top level working directory of the git repository)
  ## This is important because sbt' must be run from the top level directory
  script_dir = os.path.dirname(os.path.abspath(__file__))
  os.chdir(os.path.dirname(script_dir))

  try:
    branch=capture_output(['git', 'branch', '--show-current']).strip()
  except AbortException as e:
    abort('Cannot determine the current branch!')

  if branch != 'master':
    abort(f"Must be on 'master' branch to create a release\nCurrent branch is '{branch}'")


  if working_directory_dirty():
    print("Working directory is not clean.")
    # Show the dirty files
    system_command(['git', 'status', '--short'])
    if not getYorN('Do you wish to continue anyway?'):
      sys.exit(0)

  current_version = get_current_version()
  print(f"\nCurrent version is {current_version}")
  match re.search(r'\A(\d+)\.(\d+)\z', current_version):
    case None:
      abort('The current version does not have the correct format of <major.minor>')
    case match:
      major=match[1]
      minor=match[2]

  match new_version:
    case 'next_major':
      new_version = f'{int(major) + 1}.0'
    case 'next_minor':
      new_version = f'{major}.{int(minor) + 1}'
    case _:
      pass  # new_version was set explictly

  if getYorN(f'Set the version to {new_version} and create a release?'):
    set_version(new_version)
  else:
    sys.exit(0)

  system_command(['sbt', 'stage'], echo = True)
  update_readme(new_version)

  if args.commit:
    commit_release(new_version)
    print(f'Version {new_version} successfully created and pushed to Github!')
  else:
    print(f'Version {new_version} successfully created!')

except AbortException as e:
  print(e, file=sys.stderr)
  sys.exit(1)

