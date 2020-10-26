# Sitebuilder Sync Config

The configuration for the sync function consists of a yaml file containing a list of page configurations. If the page doesn't exist it is created, and if it does exist it is updated. Each page consists of the fields:
- `page`: The path to the page
- `content`: The path to the file containing the page content
- `properties`: (optional) A list of properties to set for the page (for a list of all options see [here](Types.md#pageoptions), the name of the property in the config is the name here without the `po` prefix)
- `files`: (optional) A list of files to upload under the page. File patterns are accepted.
- `children`: (optional) A list of child pages to this page. These follow the same structure as described here

It is important to note if you are defining subpages not as children then the subpage must come after the parent in the list, otherwise creating the pages will fail.

### Example Config

```yaml
- path: /fac/sci/dcs/materials/cs141
  content: cs141-material.html
  properties:
    spanRHS: true
    linkCaption: CS141
    pageHeading: CS141 - Functional Programming
    titleBarCaption: CS141 - Functional Programming
  files:
  - files/*
  - img/image1.png
  children:
    - path: term3/
      source: term3.html
      properties:
        spanRHS: true
- path: /fac/sci/dcs/materials/cs264
  content: cs264-material.html
```

Would create/update 3 pages:
- `/fac/sci/dcs/materials/cs141` with the content from `cs141-material.html` and with the `spanRHS` property set and the captions set as specified, and upload the file `img/image1.png` and every file under `files/`
- `/fac/sci/dcs/materials/cs141` with the content from `term3.html` and the `spanRHS` property set
- `/fac/sci/dcs/materials/cs264` with the content from `cs264-material.html`
