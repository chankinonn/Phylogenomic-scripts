import os
import shutil

# open the file, make a list of all filenames, close the file
with open('/foo/list.txt') as names_file:
# use .strip() to remove trailing whitespace and line breaks
 	names = [line.strip() for line in names_file]

	dir_src = '/foo/src'
	dir_dst = '/foo/target/'
	for file in os.listdir(dir_src):
 		if file in names:
			src_file = os.path.join(dir_src, file)
			dst_file = os.path.join(dir_dst, file)
			shutil.copy(src_file, dst_file)
