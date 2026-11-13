"""Render root guides and synchronize inst/docs. Requires Python markdown."""
from pathlib import Path
import base64, html, re, shutil
import markdown

root=Path(__file__).resolve().parents[1]
package=re.search(r'^Package:\s*(\S+)',(root/'DESCRIPTION').read_text(),re.M).group(1)
style=(root/'tools/docs.css').read_text(encoding='utf-8')
for name in ('USERGUIDE','README'):
 source=(root/(name+'.md')).read_text(encoding='utf-8')
 engine=markdown.Markdown(extensions=['tables','fenced_code','toc','sane_lists'],extension_configs={'toc':{'toc_depth':'2-3'}})
 body=engine.convert(source)
 def image(match):
  file=root/match.group(1)
  return 'src="data:image/png;base64,'+base64.b64encode(file.read_bytes()).decode()+'"'
 body=re.sub(r'src="(guide-assets/[^"]+)"',image,body)
 title=package+(' User Guide' if name=='USERGUIDE' else ' README')
 document=f'<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>{html.escape(title)}</title><style>{style}</style></head><body><nav><strong>{package}</strong><p class="subtitle">User documentation · 0.1.0</p>{engine.toc}</nav><main>{body}<footer>Documentation version 0.1.0 · November 12, 2026 · Screenshots show synthetic data.</footer></main></body></html>'
 (root/(name+'.html')).write_text(document,encoding='utf-8')
docs=root/'inst/docs';docs.mkdir(parents=True,exist_ok=True)
for name in ('USERGUIDE.md','USERGUIDE.html','README.md','README.html'):
 shutil.copyfile(root/name,docs/name)
shutil.copytree(root/'guide-assets',docs/'guide-assets',dirs_exist_ok=True)
print('Rendered and synchronized',package,'documentation.')

if (root/"LICENSE.md").exists(): shutil.copyfile(root/"LICENSE.md",docs/"LICENSE.md")
