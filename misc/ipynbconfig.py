c = get_config()
c.NbConvertApp.export_format = 'html'
c.NbConvertApp.template = 'misc/mybasic.tpl'

c.TagRemovePreprocessor.remove_cell_tags = {'noexport'} #  TODO emacs tags?

# c.Exporter.preprocessors.append('nbconvert.preprocessors.extractoutput.ExtractOutputPreprocessor')
