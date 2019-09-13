# good reference: https://github.com/jupyter/jupyter_core/blob/master/jupyter_core/tests/dotipython_empty/profile_default/ipython_nbconvert_config.py

c = get_config()
c.NbConvertApp.export_format = 'html'

c.HTMLExporter.template_file = 'misc/mybasic.tpl'

c.TagRemovePreprocessor.remove_cell_tags.add('noexport') #  TODO emacs tags?

# c.Exporter.preprocessors.append('nbconvert.preprocessors.extractoutput.ExtractOutputPreprocessor')

# TODO
# c.NbConvertApp.output_files_dir = 'NoCodeHtml'
# c.TagRemovePreprocessor.remove_input_tags.add("script")
# c.TagRemovePreprocessor.remove_input_tags.add("js_code")

