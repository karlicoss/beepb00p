# good reference: https://github.com/jupyter/jupyter_core/blob/master/jupyter_core/tests/dotipython_empty/profile_default/ipython_nbconvert_config.py

c = get_config()
c.NbConvertApp.export_format = 'html'

c.HTMLExporter.template_file = 'misc/mybasic.tpl'

c.TagRemovePreprocessor.remove_cell_tags.add('noexport') #  TODO emacs tags?

c.Exporter.preprocessors.append('nbconvert.preprocessors.extractoutput.ExtractOutputPreprocessor')

# TODO
# c.NbConvertApp.output_files_dir = 'NoCodeHtml'
# c.TagRemovePreprocessor.remove_input_tags.add("script")
# c.TagRemovePreprocessor.remove_input_tags.add("js_code")



# TODO this looks interesting vvv
# When copying files that the notebook depends on, copy them in relation to this
# path, such that the destination filename will be os.path.relpath(filename,
# relpath). If FilesWriter is operating on a notebook that already exists
# elsewhere on disk, then the default will be the directory containing that
# notebook.
# c.FilesWriter.relpath = ''

# Directory to write output to.  Leave blank to output to the current directory
# c.FilesWriter.build_directory = ''

# List of the files that the notebook references.  Files will be  included with
# written output.
# c.FilesWriter.files = []
