﻿unit Translation;

interface

uses Global, Classes, Forms, LibXmlParser, LibXmlComps, SysUtils, RegexHelper, {Unicode,} NativeXML;

procedure ListLanguages;
procedure LanguageInfo(path: string; var name, localName: string);
function LanguageAuthor(path: string): string;
procedure Add(key, value: string);
procedure LoadLanguage(path:string);
procedure LoadEnglish();
function TextByKey(key:string):string;

type
  TParser = class
    public
      constructor Create;

      procedure ListXmlScannerStartTag(Sender: TObject; TagName: string; Attributes: TAttrList);
      procedure ListXmlScannerEndTag(Sender: TObject; TagName: string);
      procedure ListXmlScannerEmptyTag(Sender: TObject; TagName: string; Attributes: TAttrList);
      procedure ListXmlScannerContent(Sender: TObject; Content: string);
    private
      currentTagname: string;
      parentTagnames: TStringList;
  end;
  
  TKeyValuePair = class
    public
      key: string;
      value: string;
  end;

  TDictionary = array of TKeyValuePair;
var
  language: TDictionary;
  defaultlanguage: TDictionary;
  tokenCount: integer;
  ListXmlScanner: TEasyXmlScanner;
  parser: TParser;

implementation

procedure ListLanguages;
var
  searchResult: TSearchRec;
begin
  if FindFirst(ExtractFilePath(Application.ExeName) + 'Languages\*.xml', faAnyFile, searchResult) = 0 then
  begin
    repeat
      AvailableLanguages.Add(ExtractFilePath(Application.ExeName) + 'Languages\' + searchResult.Name);
    until (FindNext(searchResult) <> 0);
    SysUtils.FindClose(searchResult);
  end;
end;

procedure LanguageInfo(path: string; var name, localName: string);
const
  exp1 = '\stitle="([^"]*)"';
  exp2 = '\slocalized-title="([^"]*)"';
var
  langfile : TextFile;
  buffer, langxml : string;
begin
  AssignFile(langfile, path) ;
  Reset(langfile) ;
  while not EOF(langfile) do
    begin
      ReadLn(langfile, buffer) ;
      langxml := langxml + #13#10 + buffer;
    end;
  CloseFile(langfile) ;

  name := GetStringPart(langxml, exp1, 1, '');
  localname := GetStringPart(langxml, exp2, 1, '');
end;
function LanguageAuthor(path: string): string;
const
  exp = '\sauthor="([^"]*)"';
var
  langfile : TextFile;
  buffer, langxml : string;
begin
  AssignFile(langfile, path) ;
  Reset(langfile) ;
  while not EOF(langfile) do
    begin
      ReadLn(langfile, buffer) ;
      langxml := langxml + #13#10 + buffer;
    end;
  CloseFile(langfile) ;

  Result := GetStringPart(langxml, exp, 1, '');
end;

procedure LoadEnglish();
begin
  //TODO:
    //render-status-renderhibernated

  Add('common-ok', 'OK');
	Add('common-cancel', 'Cancel');
	Add('common-apply', 'Apply');
  Add('common-yes', 'Yes');
	Add('common-no', 'No');
	Add('common-lowquality', 'Low Quality');
	Add('common-mediumquality', 'Medium Quality');
	Add('common-highquality', 'High Quality');
	Add('common-copy', 'Copy');
	Add('common-paste', 'Paste');
	Add('common-delete', 'Delete');
	Add('common-rename', 'Rename');
	Add('common-undo', 'Undo');
	Add('common-redo', 'Redo');
	Add('common-width', 'Width');
	Add('common-height', 'Height');
	Add('common-size', 'Size');
	Add('common-pixels', 'Pixels');
	Add('common-keepaspect', 'Maintain aspect ratio');
	Add('common-destination', 'Destination');
	Add('common-filename', 'File name');
	Add('common-browse', 'Browse...');
	Add('common-quality', 'Quality');
	Add('common-filterradius', 'Filter Radius');
	Add('common-density', 'Density');
	Add('common-oversample', 'Oversample');
	Add('common-gamma', 'Gamma');
	Add('common-brightness', 'Brightness');
	Add('common-contrast', 'Contrast');
	Add('common-vibrancy', 'Vibrancy');
	Add('common-background', 'Background');
	Add('common-gammathreshold', 'Gamma Threshold');
	Add('common-start', 'Start');
	Add('common-pause', 'Pause');
  Add('common-resume', 'Resume');
	Add('common-close', 'Close');
	Add('common-clear', 'Clear');
	Add('common-enabled', 'Enabled');
	Add('common-disabled', 'Disabled');
	Add('common-minimum', 'Minimum');
	Add('common-maximum', 'Maximum');
  Add('common-resetlocation', 'Reset location');
  Add('common-genericopenfailure', 'Unable to open "%s".');
	Add('common-genericsavefailure', 'Unable to save "%s".');
  Add('common-invalidformat', 'Invalid format.');
	Add('common-confirmexit', 'Do you really want to exit? All unsaved data will be lost!');
  Add('common-confirmdelete', 'Are you sure you want to permanently delete "%s"?');
	Add('common-dragpanelhint', 'Click and drag to change value');
  Add('common-trace-pausing', 'Pausing render');
	Add('common-trace-resuming', 'Resuming render');
	Add('common-trace-terminating', 'Terminating render');
	Add('common-trace-stopping', 'Stopping render');
	Add('common-trace-saving', 'Saving image');
	Add('common-trace-creating-simple', 'Creating image');
	Add('common-trace-creating-detailed', 'Creating image with quality: %f');
	Add('common-trace-rendering-oneslice', 'Rendering');
	Add('common-trace-rendering-multipleslices', 'Rendering slice %d of %d');
	Add('common-trace-allocating', 'Allocating histogram (%n MB)');
	Add('common-trace-notenoughmemory', 'Not enough memory for this render!');
	Add('common-statistics-title-oneslice', 'Render Statistics:');
	Add('common-statistics-title-multipleslices', 'Render Statistics for the last slice:');
	Add('common-statistics-maxpossiblebits', 'Max possible bits: %2.3f');
	Add('common-statistics-maxred', 'Max Red:   %2.3f bits');
	Add('common-statistics-maxgreen', 'Max Green: %2.3f bits');
	Add('common-statistics-maxblue', 'Max Blue:  %2.3f bits');
	Add('common-statistics-maxcounter', 'Max Count: %2.3f bits');
	Add('common-statistics-pointhitratio', 'Point hit ratio: %2.2f%%');
	Add('common-statistics-averagespeed', 'Average speed: %n iterations per second');
	Add('common-statistics-purerenderingtime', 'Pure rendering time:');
  Add('common-seconds', 'second(s)');
  Add('common-minutes', 'minute(s)');
  Add('common-hours', 'hour(s)');
  Add('common-days', 'day(s)');
  Add('common-suspendtodisk', 'Suspend to disk');
  Add('common-resumefromdisk', 'Resume from disk');
  Add('common-filter-flamefiles', 'Apophysis Flame Library (*.flame;*.xml)');
	Add('common-filter-gradientfiles', 'Gradient files (*.gradient;*.ugr)');
  Add('common-filter-fractintfiles', 'Fractint maps (*.map)');
	Add('common-filter-uprfiles', 'UltraFractal parameters (*.upr)');
	Add('common-filter-script', 'Apophysis Script (*.aposcript;*.asc)');
  Add('common-filter-allimages', 'All images (*.bmp;*.dib;*.jpg;*.jpeg)');
	Add('common-filter-bitmap', 'Windows Bitmap (*.bmp;*.dib)');
	Add('common-filter-jpeg', 'JPEG (*.jpg;*.jpeg)');
	Add('common-filter-png', 'Portable Network Graphics (*.png)');
	Add('common-filter-allfiles', 'All files (*.*)');
  Add('splash-loadingui', 'Loading user interface...');
	Add('splash-loadingsettings', 'Loading settings...');
	Add('splash-loadingplugins', 'Loading plugins...');
	Add('splash-initrenderer', 'Initializing renderer...');
	Add('splash-initcolormap', 'Initializing gradients...');
	Add('splash-initbatch', 'Creating random batch...');
	Add('splash-execstartupscript', 'Executing "autoexec.asc"...');
	Add('adjustment-title', 'Adjustment');
	Add('adjustment-common-depthblur', 'Depth Blur');
	Add('adjustment-common-pitch', 'Pitch');
	Add('adjustment-common-yaw', 'Yaw');
	Add('adjustment-common-height', 'Height');
	Add('adjustment-common-perspective', 'Perspective');
	Add('adjustment-common-scale', 'Scale');
	Add('adjustment-tab-camera-title', 'Camera');
	Add('adjustment-tab-camera-zoom', 'Zoom');
	Add('adjustment-tab-camera-xpos', 'X-Position');
	Add('adjustment-tab-camera-ypos', 'Y-Position');
	Add('adjustment-tab-camera-rotation', 'Rotation');
	Add('adjustment-tab-rendering-title', 'Rendering');
	Add('adjustment-tab-rendering-istransparent', 'Transparent');
	Add('adjustment-tab-gradient-title', 'Gradient');
	Add('adjustment-tab-gradient-moderotate', 'Rotate');
	Add('adjustment-tab-gradient-modehue', 'Hue');
	Add('adjustment-tab-gradient-modesaturation', 'Saturation');
	Add('adjustment-tab-gradient-modebrightness', 'Brightness');
	Add('adjustment-tab-gradient-modecontrast', 'Contrast');
	Add('adjustment-tab-gradient-modeblur', 'Blur');
	Add('adjustment-tab-gradient-modefrequency', 'Frequency');
	Add('adjustment-tab-gradient-preset', 'Preset');
	Add('adjustment-tab-gradient-reset', 'Reset');
	Add('adjustment-tab-gradient-modehint', 'Click for menu');
	Add('adjustment-tab-gradient-presethint', 'Click to choose random preset');
	Add('adjustment-tab-size-title', 'Image size');
	Add('adjustment-tab-size-preset', 'Empty preset');
	Add('adjustment-tab-size-resizemain', 'Resize main window');
	Add('adjustment-popup-quality-instantpreview', 'Instant preview');
	Add('adjustment-popup-gradient-randomize', 'Randomize');
	Add('adjustment-popup-gradient-invert', 'Invert');
	Add('adjustment-popup-gradient-reverse', 'Reverse');
	Add('adjustment-popup-gradient-smooth', 'Smooth Palette...');
	Add('adjustment-popup-gradient-browser', 'Gradient Browser...');
	Add('adjustment-popup-gradient-saveasugr', 'Save Gradient...');
	Add('adjustment-popup-gradient-saveasmap', 'Save as Map File...');
	Add('adjustment-popup-gradient-saveasdefault', 'Save as default');
  Add('editor-title', 'Editor');
	Add('editor-common-transform', 'Transform:');
	Add('editor-common-name', 'Name:');
	Add('editor-common-weight', 'Weight:');
  Add('editor-common-finalxformlistitem', 'Final');
	Add('editor-common-fromprefix', 'from %d');
	Add('editor-common-toprefix', 'to %d');
	Add('editor-tab-variations-title', 'Variations');
	Add('editor-tab-variations-name', 'Name');
	Add('editor-tab-variations-value', 'Value');
	Add('editor-tab-variations-togglehideunused', 'Hide unused variations');
	Add('editor-tab-variables-title', 'Variables');
	Add('editor-tab-variables-name', 'Name');
	Add('editor-tab-variables-value', 'Value');
	Add('editor-tab-variables-toggleshowall', 'Show all variables');
	Add('editor-tab-chaos-title', 'Xaos');
	Add('editor-tab-chaos-path', 'Path');
	Add('editor-tab-chaos-modifier', 'Weight modifier');
	Add('editor-tab-chaos-viewasto', 'View as "to"');
	Add('editor-tab-chaos-viewasfrom', 'View as "from"');
	Add('editor-tab-triangle-title', 'Triangle');
	Add('editor-tab-triangle-pivot', 'Pivot');
	Add('editor-tab-triangle-modelocal', 'Local');
	Add('editor-tab-triangle-modeworld', 'World');
	Add('editor-tab-triangle-resetpivot', 'Reset pivot point');
	Add('editor-tab-triangle-pickpivot', 'Pick pivot point');
	Add('editor-tab-triangle-rotateleft', 'Rotate triangle counter-clockwise');
	Add('editor-tab-triangle-rotateright', 'Rotate triangle clockwise');
	Add('editor-tab-triangle-moveup', 'Move triangle up');
	Add('editor-tab-triangle-movedown', 'Move triangle down');
	Add('editor-tab-triangle-moveleft', 'Move triangle left');
	Add('editor-tab-triangle-moveright', 'Move triangle right');
	Add('editor-tab-triangle-scaledown', 'Scale triangle down');
	Add('editor-tab-triangle-scaleup', 'Scale triangle up');
	Add('editor-tab-triangle-autoweight', 'Auto-balance weights');
	Add('editor-tab-transform-title', 'Transform');
	Add('editor-tab-transform-reset', 'Reset transform');
	Add('editor-tab-transform-resethint', 'Reset all vectors to default position');
	Add('editor-tab-transform-rectangular', 'Rectangular');
	Add('editor-tab-transform-rectangularhint', 'Show vectors in rectangular (cartesian) corrdinates');
	Add('editor-tab-transform-polar', 'Polar (deg)');
	Add('editor-tab-transform-polarhint', 'Show vector in polar coordinates');
	Add('editor-tab-transform-resetpost', 'Reset post transform');
	Add('editor-tab-transform-resetposthint', 'Reset all post-transform vectors to default position');
	Add('editor-tab-transform-autozscale', 'Auto-calculate pre_zscale');
	Add('editor-tab-transform-resetxhint', 'Reset vector X');
	Add('editor-tab-transform-resetyhint', 'Reset vector Y');
	Add('editor-tab-transform-resetohint', 'Reset vector O');
	Add('editor-tab-color-title', 'Colors');
	Add('editor-tab-color-transformcolor', 'Transform color');
	Add('editor-tab-color-colorspeed', 'Color speed');
	Add('editor-tab-color-opacity', 'Opacity');
	Add('editor-tab-color-directcolor', 'Direct color');
	Add('editor-tab-color-togglesolo', 'Solo');
  Add('editor-tab-color-togglesoloformat', 'Solo transform #%d');
	Add('editor-tab-color-varpreview', 'Variation preview');
	Add('editor-tab-color-previewrange', 'Range');
	Add('editor-tab-color-previewdepth', 'Depth');
	Add('editor-tab-color-previewdensity', 'Density');
  Add('editor-tab-color-preview', 'Preview');
	Add('editor-toolbar-newflame', 'New flame');
	Add('editor-toolbar-newtransform', 'New transform');
	Add('editor-toolbar-addlinkedtransform', 'Add linked transform');
	Add('editor-toolbar-duplicatetransform', 'Duplicate transform');
	Add('editor-toolbar-removetransform', 'Remove transform');
	Add('editor-toolbar-modeselect', 'Selection mode');
	Add('editor-toolbar-modemove', 'Movement mode');
	Add('editor-toolbar-moderotate', 'Rotation mode');
	Add('editor-toolbar-modescale', 'Scale mode');
	Add('editor-toolbar-toggleworldpivot', 'Toggle world pivot');
	Add('editor-toolbar-rotate90ccw', 'Rotate 90° counter-clockwise');
	Add('editor-toolbar-rotate90cw', 'Rotate 90° clockwise');
	Add('editor-toolbar-fliph', 'Flip horizontal');
	Add('editor-toolbar-flipv', 'Flip vertical');
	Add('editor-toolbar-togglevarpreview', 'Show variation preview');
	Add('editor-toolbar-toggleposttransform', 'Enable/edit post-transform');
	Add('editor-toolbar-togglefinaltransform', 'Enable final transform');
	Add('editor-popup-panel-autozoom', 'Zoom automatically');
	Add('editor-popup-panel-toggleextendededit', 'Toggle extended edit mode');
	Add('editor-popup-panel-locktransformaxes', 'Lock transform axes');
	Add('editor-popup-panel-allfliph', 'Flip all horizontally');
	Add('editor-popup-panel-allflipv', 'Flip all vertically');
	Add('editor-popup-quality-autoreset', 'Auto-reset location');
	Add('editor-popup-transform-resetposition', 'Reset position');
	Add('editor-popup-transform-resetrotation', 'Reset rotation');
	Add('editor-popup-transform-resetscale', 'Reset scale');
	Add('editor-popup-transform-copycoords', 'Copy triangle coordinates');
	Add('editor-popup-transform-pastecoords', 'Paste triangle coordinates');
	Add('editor-popup-transform-resetentiretriangle', 'Reset triangle');
	Add('editor-popup-chaos-rebuildlinks', 'Rebuild links');
	Add('editor-popup-chaos-clearall', 'Clear all');
	Add('editor-popup-chaos-setall', 'Set all');
  Add('editor-status-zoomformat', 'Zoom: %f');
	Add('editor-status-xformat', 'X: %f');
	Add('editor-status-yformat', 'Y: %f');
	Add('editor-status-rotateformat', 'Rotate: %3.2f°  Inner angle: %3.2f°');
	Add('editor-status-rotateformat2', 'Rotate: %3.2f°  Local axis: %3.2f°');
	Add('editor-status-rotateformat3', 'Rotate: %3.2f°');
	Add('editor-status-scaleformat', 'Distance: %3.3f  Scale: %3.2f%%');
	Add('editor-status-scaleformat2', 'Scale: %3.2f%%');
	Add('editor-status-moveformat', 'Move: %3.3f ; %3.3f');
	Add('editor-status-moveformat2', 'Move: %3.3f ; %3.3f');
	Add('editor-status-transformformat', 'Transform #%d');
	Add('editor-status-zoomformat', 'Zoom: %f');
	Add('editor-status-selecton', 'Select ON');
	Add('editor-status-selectoff', 'Select OFF');
	Add('export-title', 'Export to flam3');
	Add('export-paramoptions-title', 'Parameter options');
	Add('export-paramoptions-bufferdepth', 'Buffer depth');
	Add('export-paramoptions-strips', 'Strips');
	Add('export-paramoptions-estimatorradius', 'DE radius');
	Add('export-paramoptions-estimatorcurve', 'DE curve');
	Add('export-paramoptions-estimatormin', 'DE minimum');
	Add('export-paramoptions-dorender', 'Render');
	Add('export-paramoptions-warningtitle', 'WARNING');
	Add('export-paramoptions-warningtext', 'Fractals created with this version of Apophysis are not supported by flam3! To render 2D-only fractals, download the latest version of flam3 from http://www.flam3.com');
	Add('favscripts-title', 'Favorite scripts');
	Add('favscripts-add', 'Add');
	Add('favscripts-remove', 'Remove');
	Add('favscripts-moveup', 'Move up');
	Add('favscripts-movedown', 'Move down');
  Add('fullscreen-popup-rendermore', 'Render more');
	Add('fullscreen-popup-stoprender', 'Stop render');
  Add('gradientbrowser-title', 'Gradient Browser');
	Add('postprocess-title', 'Post-process render');
  Add('postprocess-save', 'Save');
  Add('postprocess-fittowindow', 'Fit to window');
	Add('render-title', 'Render flame');
	Add('render-common-gotofolder', 'Open target folder...');
	Add('render-tab-settings-title', 'Settings');
	Add('render-tab-output-title', 'Output');
	Add('render-resourceusage-title', 'Resource usage');
	Add('render-resourceusage-infotext', 'The render process will use %u MB of %u MB available physical memory');
	Add('render-resourceusage-limit', 'Memory limit');
	Add('render-resourceusage-nolimit', 'No limit');
	Add('render-resourceusage-bufferdepth', 'Buffer depth');
	Add('render-output-title', 'Output options');
	Add('render-output-saveparams', 'Save parameters');
	Add('render-output-writeexif', 'Write EXIF-Header (JPEG only)');
	Add('render-output-author', 'Author (EXIF)');
	Add('render-output-includeparams', 'Include parameters in EXIF-Header');
	Add('render-completion-title', 'Completion options');
	Add('render-completion-postprocess', 'Post-process after rendering');
	Add('render-completion-shutdown', 'Shut down after rendering');
	Add('render-completion-saveincomplete', 'Save incomplete renders');
  Add('render-status-rendererror-log', 'Rendering failed!');
	Add('render-status-rendererror-message', 'Error while rendering!');
	Add('render-status-saveerror-log', 'Error saving image!');
	Add('render-status-saveerror-message1', 'An error occured while saving the image:');
	Add('render-status-saveerror-message2', 'Check your free disk space and try again.');
	Add('render-status-totaltime', 'Total time:');
	Add('render-status-renderterminated', 'Rendering terminated!');
	Add('render-status-renderhibernated', 'Rendering paused and progress saved!');
	Add('render-status-elapsed', 'Elapsed');
	Add('render-status-remaining', 'Remaining');
	Add('render-status-slicestatus', 'Slice %d of %d');
	Add('render-status-notenoughmemory1', 'You do not have enough memory for this render. Please use memory limiting.');
	Add('render-status-notenoughmemory2', 'You do not have enough memory for this render. Please use a lower Maximum memory setting.');
	Add('render-status-nofilename', 'Please enter a file name.');
	Add('render-status-fileexists-message1', '"%s" already exists');
	Add('render-status-fileexists-message2', 'Do you want to replace it?');
	Add('render-status-pathdoesnotexist', 'The directory does not exist.');
	Add('render-status-invaliddensity', 'Invalid Sample Density value');
	Add('render-status-invalidfilterradius', 'Invalid Filter Radius value');
	Add('render-status-invalidoversample', 'Invalid Oversample value');
	Add('render-status-invalidwidth', 'Invalid image width');
	Add('render-status-invalidheight', 'Invalid image height');
	Add('render-status-maxmemorytoosmall', 'Maximum memory value is too small');
	Add('render-status-shuttingdownrender', 'Shutting down previous render...');
	Add('render-status-log-title', 'Rendering "%s"');
	Add('render-status-log-size', 'Size: %dx%d');
	Add('render-status-log-quality', 'Quality: %g');
	Add('render-status-log-oversampling', 'Oversample: %d, Filter: %g');
	Add('render-status-log-bufferdepth', 'Buffer depth: %s');
	Add('render-status-log-memorylimit', 'Memory limit: %d MB');
	Add('render-status-log-largepng-message1', '*** WARNING *** You have selected PNG format and an image size which exceeds 20 megapixels');
	Add('render-status-log-largepng-message2', 'PNG format with extreme high-resolution images is not recommended!');
	Add('render-status-log-largepng-message3', 'To avoid slowdown (and possible memory problems) use BMP file format instead.');
	Add('render-status-confirmstop', 'Do you want to stop the current render?');
	Add('imagecoloring-title', 'Image coloring');
	Add('imagecoloring-enable', 'Enable');
	Add('imagecoloring-firstpalette', 'First palette');
	Add('imagecoloring-secondpalette', 'Second palette');
	Add('imagecoloring-preset', 'Preset');
	Add('imagecoloring-image', 'Image');
	Add('messages-title', 'Messages');
	Add('messages-openautomatically', 'Automatically open this window');
	Add('mutation-title', 'Mutation');
	Add('mutation-directions', 'Directions');
	Add('mutation-speed', 'Speed');
	Add('mutation-trend', 'Trend');
	Add('mutation-keepnumberoftransforms', 'Keep transform count');
  Add('mutation-randomtrend', 'Random');
	Add('mutation-maintainsymmetry', 'Maintain symmetry');
	Add('mutation-previous', 'Previous');
  Add('options-title', 'Settings ');
	Add('options-restartnotice', 'You must restart Apophysis 7x to make your changes have effect');
	Add('options-tab-general-title', 'General ');
  Add('options-tab-general-language', 'Language file');
	Add('options-tab-general-multithreading', 'Multithreading ');
	Add('options-tab-general-multithreading-off', 'Off ');
	Add('options-tab-general-bufferdepth', 'Buffer depth ');
	Add('options-tab-general-jpegquality', 'JPEG quality ');
	Add('options-tab-general-pngtransparency', 'PNG transparency ');
	Add('options-tab-general-showextendedstatistics', 'Show extended render statistics ');
	Add('options-tab-general-confirmdelete', 'Confirm delete ');
	Add('options-tab-general-confirmexit', 'Confirm exit ');
	Add('options-tab-general-confirmrenderstop', 'Confirm stop rendering ');
	Add('options-tab-general-oldgradientformat', 'Use old gradient format ');
	Add('options-tab-general-alwaysblankflame', 'Disable templates ');
	Add('options-tab-general-enablemissingpluginswarning', 'Warn on missing plugins ');
	Add('options-tab-general-enablethumbnailembedding', 'Enable thumbnail embedding ');
	Add('options-tab-general-rotatemode', 'Rotate mode ');
	Add('options-tab-general-rotateimage', 'Rotate image ');
	Add('options-tab-general-rotateframe', 'Rotate frame ');
	Add('options-tab-general-zoommode', 'Zoom mode ');
	Add('options-tab-general-preservequality', 'Preserve quality ');
	Add('options-tab-general-preservespeed', 'Preserve speed ');
	Add('options-tab-general-guides', 'Guidelines ');
	Add('options-tab-general-enableguides', 'Enable guides ');
	Add('options-tab-general-guidecentercolor', 'Center ');
	Add('options-tab-general-guidethirdscolor', 'Thirds ');
	Add('options-tab-general-guidegoldenratiocolor', 'Golden ratio ');
	Add('options-tab-editor-title', 'Editor ');
	Add('options-tab-editor-editorgraph', 'Graph ');
	Add('options-tab-editor-editordefaults', 'Defaults ');
	Add('options-tab-editor-referencetriangle', 'Reference triangle ');
	Add('options-tab-editor-usetransformcolor', 'Use transform color ');
	Add('options-tab-editor-helperlines', 'Show helper lines ');
	Add('options-tab-editor-alwaysshowbothtransformtypes', 'Always show both transform types ');
	Add('options-tab-editor-backgroundcolor', 'Background ');
	Add('options-tab-editor-gridcolors', 'Grid ');
	Add('options-tab-editor-referencecolor', 'Reference ');
	Add('options-tab-editor-helpercolors', 'Helpers ');
	Add('options-tab-editor-extendededit', 'Extended edit mode ');
	Add('options-tab-editor-locktransformaxes', 'Lock transform axes ');
	Add('options-tab-editor-rebuildxaoslinks', 'Rebuild links ');
	Add('options-tab-editor-normalreference', 'Normal ');
	Add('options-tab-editor-proportionalreference', 'Proportional ');
	Add('options-tab-editor-wanderingreference', 'Wandering (old style) ');
  Add('options-tab-editor-enablepreview', 'Enable editor preview');
  Add('options-tab-editor-previewtransparency', 'Transparency');
	Add('options-tab-display-title', 'Display ');
	Add('options-tab-display-rendering', 'Rendering ');
	Add('options-tab-display-previewdensity', 'Preview density ');
	Add('options-tab-display-mainpreview', 'Main window preview ');
	Add('options-tab-display-extendpreviewbuffer', 'Extend preview buffer ');
	Add('options-tab-display-extenspreviewbufferlabel', 'Buffer extension ');
	Add('options-tab-display-showtransparency', 'Show transparency ');
	Add('options-tab-display-usesmallthumbs', 'Use small thumbnails (like Apophysis 2.09) ');
	Add('options-tab-random-title', 'Random ');
	Add('options-tab-random-numberoftransforms', 'Number of transforms ');
	Add('options-tab-random-mutationtransforms', 'Mutation transforms ');
	Add('options-tab-random-randombatch', 'Random batch ');
	Add('options-tab-random-forcedsymmetry', 'Forced symmetry ');
	Add('options-tab-random-batchsize', 'Batch size ');
	Add('options-tab-random-titleprefix', 'Title prefix ');
	Add('options-tab-random-keepbackground', 'Keep background color ');
	Add('options-tab-random-symtype', 'Type ');
	Add('options-tab-random-symorder', 'Order ');
	Add('options-tab-random-symlimit', 'Limit ');
	Add('options-tab-random-type-none', 'None ');
	Add('options-tab-random-type-bilateral', 'Bilateral ');
	Add('options-tab-random-type-rotational', 'Rotational ');
	Add('options-tab-random-type-dihedral', 'Dihedral ');
	Add('options-tab-random-onrandom', 'On random flame use... ');
	Add('options-tab-random-userandom', 'Random preset ');
	Add('options-tab-random-usedefault', 'Default gradient ');
	Add('options-tab-random-usecurrent', 'Current gradient ');
	Add('options-tab-random-randomcalculated', 'Random gradient ');
	Add('options-tab-random-randomfromfile', 'Random from file ');
	Add('options-tab-random-filetouse', 'Random file to use ');
	Add('options-tab-variations-title', 'Variations ');
	Add('options-tab-variations-setall', 'Set all ');
	Add('options-tab-variations-clearall', 'Clear all ');
	Add('options-tab-gradient-title', 'Gradient ');
	Add('options-tab-gradient-numberofnodes', 'Number of nodes ');
	Add('options-tab-gradient-smoothpalette', 'Smooth palette ');
	Add('options-tab-gradient-huebetween', 'Hue range ');
	Add('options-tab-gradient-satbetween', 'Saturation range ');
	Add('options-tab-gradient-lumbetween', 'Luminance range ');
	Add('options-tab-gradient-numtries', 'Number of tries ');
	Add('options-tab-gradient-trylength', 'Try length ');
	Add('options-tab-upr-title', 'UltraFractal ');
	Add('options-tab-upr-paramdefaults', 'Parameter defaults ');
	Add('options-tab-upr-coloralgorithm', 'Coloring algorithm ');
	Add('options-tab-upr-uprsize', 'Image size ');
	Add('options-tab-upr-formula', 'Formula ');
	Add('options-tab-upr-identifier', 'Identifier ');
	Add('options-tab-upr-adjustdensity', 'Adjust sample density ');
	Add('options-tab-environment-title', 'Environment');
	Add('options-tab-environment-defaultparams', 'Default parameters ');
	Add('options-tab-environment-smoothpalette', 'Smooth palette ');
	Add('options-tab-environment-functionlib', 'Scripting function library ');
	Add('options-tab-environment-exportrenderer', 'Export renderer ');
	Add('options-tab-environment-helpfile', 'Help file ');
	Add('options-tab-environment-rememberlastopen', 'Remember last open parameters ');
	Add('options-tab-environment-autosave', 'Enable autosave ');
	Add('options-tab-environment-savefrequency', 'Save frequency ');
  Add('options-tab-environment-usex64chaotica', 'Use 64-bit version if possible');
	Add('preview-title', 'Preview');
	Add('save-title', 'Save');
	Add('save-name', 'Name');
  Add('save-oldformat', 'Use old format');
  Add('save-newformat', 'Use new format');
  Add('save-type-parameters', 'Save Parameters');
	Add('save-type-allparameters', 'Save All Parameters');
	Add('save-type-gradient', 'Save Gradient');
	Add('save-type-exportupr', 'Export UPR');
	Add('save-status-notitle', 'No item name given.');
	Add('save-status-invalidfilename', 'Invalid file name.');
	Add('save-status-alreadyexists', '"%s" in "%s" already exists. Do you want to replace it?');
	Add('save-status-alreadyexists2', '"%s" already exists. Do you want to replace it?');
	Add('savepreset-title', 'Save preset');
	Add('savepreset-name', 'Name');
  Add('savepreset-notitle', 'No preset name given.');
	Add('script-title', 'Script');
  Add('script-rendering', 'Rendering...');
  Add('script-break', 'Break');
	Add('script-new', 'New');
	Add('script-open', 'Open');
	Add('script-save', 'Save');
	Add('script-run', 'Run');
	Add('script-stop', 'Stop');
	Add('splash-loadingtext', 'Loading');
	Add('template-title', 'Templates');
  Add('main-common-title-lite', 'Lite Version');
	Add('main-common-title-t500', 'High-Memory Version');
	Add('main-common-randombatch', 'Random Batch');
  Add('main-menu-file-title', 'File');
	Add('main-menu-file-new', 'New');
	Add('main-menu-file-open', 'Open...');
	Add('main-menu-file-restoreautosave', 'Restore last autosave');
	Add('main-menu-file-saveparams', 'Save parameters...');
	Add('main-menu-file-saveallparams', 'Save all parameters...');
	Add('main-menu-file-smoothpalette', 'Smooth palette...');
	Add('main-menu-file-gradientbrowser', 'Gradient browser...');
	Add('main-menu-file-exportupr', 'Export UPR...');
	Add('main-menu-file-exportflame', 'Export to flam3...');
	Add('main-menu-file-exportchaotica', 'Export to Chaotica...');
	Add('main-menu-file-importgimp', 'Import GIMP parameters...');
	Add('main-menu-file-submitsheep', 'Submit sheep');
	Add('main-menu-file-randombatch', 'Random batch');
	Add('main-menu-file-exit', 'Exit');
	Add('main-menu-edit-title', 'Edit');
	Add('main-menu-edit-saveundo', 'Save undo stack...');
	Add('main-menu-edit-copyasupr', 'Copy as UPR');
	Add('main-menu-view-title', 'View');
	Add('main-menu-view-fullscreen', 'Full screen');
	Add('main-menu-view-editor', 'Editor');
	Add('main-menu-view-adjustment', 'Adjustment');
	Add('main-menu-view-gradient', 'Gradient');
	Add('main-menu-view-mutation', 'Mutation');
	Add('main-menu-view-imagesize', 'Image size');
	Add('main-menu-view-messages', 'Messages');
	Add('main-menu-flame-title', 'Flame');
	Add('main-menu-flame-reset', 'Reset location');
	Add('main-menu-flame-randomize', 'Randomize');
	Add('main-menu-flame-randomweights', 'Randomize weights');
	Add('main-menu-flame-equalweights', 'Equalize weights');
	Add('main-menu-flame-computeweights', 'Normalize weights');
	Add('main-menu-flame-calculatecolors', 'Calculate colors');
	Add('main-menu-flame-randomizecolors', 'Randomize colors');
	Add('main-menu-flame-rendertodisk', 'Render flame...');
	Add('main-menu-flame-renderallflames', 'Render all flames...');
  Add('main-menu-flame-resumeunfinished', 'Resume unfinished render process...');
	Add('main-menu-flame-generatereport', 'Summarize flame...');
	Add('main-menu-variation-title', 'Variation');
	Add('main-menu-variation-random', 'Random');
	Add('main-menu-variation-builtin', 'Built-in');
	Add('main-menu-variation-plugins', 'Plugins');
	Add('main-menu-script-title', 'Script');
	Add('main-menu-script-run', 'Run script');
  Add('main-menu-script-run2', 'Run "%s"');
	Add('main-menu-script-directory', 'Directory');
	Add('main-menu-script-more', 'More');
	Add('main-menu-script-stop', 'Stop script');
	Add('main-menu-script-open', 'Open...');
	Add('main-menu-script-edit', 'Edit script');
	Add('main-menu-script-managefaves', 'Manage favorites...');
	Add('main-menu-script-flametoscript', 'Generate script from flame');
	Add('main-menu-options-title', 'Tools');
	Add('main-menu-options-togglemaintoolbar', 'Show toolbar');
	Add('main-menu-options-togglestatusbar', 'Show status bar');
	Add('main-menu-options-togglefilelist', 'Show parameter list');
	Add('main-menu-options-resetfilelistwidth', 'Reset layout');
	Add('main-menu-options-showoptions', 'Settings...');
	Add('main-menu-help-title', '?');
	Add('main-menu-help-contents', 'Contents');
	Add('main-menu-help-aboutalgorithm', 'About fractal flames...');
	Add('main-menu-help-aboutapophysis', 'About Apophysis 7x...');
	Add('main-toolbar-listviewmode-classic', 'Classic view');
	Add('main-toolbar-listviewmode-icons', 'Thumbnail view');
	Add('main-toolbar-togglealpha', 'Show transparency');
	Add('main-toolbar-toggleguides', 'Show guidelines');
	Add('main-toolbar-modemove', 'Pan camera');
	Add('main-toolbar-moderotate', 'Rotate camera');
	Add('main-toolbar-modezoomin', 'Zoom in');
	Add('main-toolbar-modezoomout', 'Zoom out');
  Add('main-status-batchgenerate', 'Generating %d of %s...');
	Add('main-status-batcherror', 'Error creating batch.');
	Add('main-status-calculatingpalette', 'Calculating palette (%d%)...');
	Add('main-status-noflam3', 'Unable to find flam3 executable. Please verify your settings.');
	Add('main-status-nohelpfile', 'Please specify a help file path in the options dialog first.');
	Add('main-status-variationsorvariables', 'variations or variables');
	Add('main-status-plugins', 'plugins');
	Add('main-status-noloadingerrors', 'Flame loaded without errors');
	Add('main-status-loadingerrorcount', '%d errors in flame');
	Add('main-status-morepluginsneeded', 'The flame "%s" requires the following additional %s:');
  Add('main-status-noautosave', 'No autosave present.');
  Add('main-status-chaoticacompatmissing', 'The variation compatibility data file can not be found at the configured location of Chaotica. The rendering result may look different from the preview. Do you want to proceed?');
  Add('main-status-nochatocia', 'The executable file of Chaotica could not be found. Please check your settings.');
  Add('main-status-oldchaotica', 'It seems you are using a version of Chaotica prior than 0.3. The rendering result may look different from the preview. Do you want to proceed?');
	Add('main-report-transformcount', 'Transform count: %d');
	Add('main-report-finaltransform', 'Has final transform: %s');
	Add('main-report-usedplugins', 'Used plugins:');
	Add('main-report-noplugins', '(none)');
end;

procedure Add(key, value: string);
var entry : TKeyValuePair;
begin
  Inc(tokenCount);
  SetLength(language, tokenCount);
  SetLength(defaultlanguage, tokenCount);
  entry := TKeyValuePair.Create;
  entry.key := key;
  entry.value := value;
  language[tokenCount - 1] := entry;
  defaultlanguage[tokenCount - 1] := entry;
end;
procedure AddNoDefault(key, value: string);
var entry : TKeyValuePair;
begin
  Inc(tokenCount);
  SetLength(language, tokenCount);
  entry := TKeyValuePair.Create;
  entry.key := key;
  entry.value := value;
  language[tokenCount - 1] := entry;
end;
procedure AddNodes(node: TXMLNode; keyName: UTF8string);
var i: integer; newName: UTF8String;
begin
  for i:=0 to node.NodeCount - 1 do begin
    if (node.Name <> UTF8String('stringtable')) then
      newName := keyName + node.Name + UTF8String('-')
    else newName := keyName;
    AddNodes(node.Nodes[i], newName);
  end;
  if node.ValueAsString <> UTF8String('') then begin
    Add(string(keyName + node.Name), string(node.ValueAsString));
  end;
end;

procedure LoadLanguage(path:string);
var
  document : TNativeXML;
begin
  if (path = '') or (not FileExists(path)) then LoadEnglish()
  else begin
    tokenCount := 0;

    document := TNativeXML.Create;
    document.LoadFromFile(path);;
    if lowercase(string(document.EncodingString)) <> 'utf-8' then begin
      AddNodes(document.Root, UTF8String(''));
    end else begin  // use easy xml because it supports utf-8 properly (at least...)
      parser := TParser.Create;
      ListXmlScanner := TEasyXmlScanner.Create(nil);

      ListXmlScanner.OnStartTag := parser.ListXmlScannerStartTag;
      ListXmlScanner.OnEndTag := parser.ListXmlScannerEndTag;
      ListXmlScanner.OnEmptyTag := parser.ListXmlScannerEmptyTag;
      ListXmlScanner.OnContent := parser.ListXmlScannerContent;

      ListXmlScanner.Filename := path;
      ListXmlScanner.Execute;

      ListXmlScanner.Destroy;
      parser.Destroy;
    end;
  end;
end;

function TextByKey(key:string):string;
var i: integer;
begin

  Result := '#ERR_NO_TEXT#';
  for i:=0 to tokenCount - 1 do begin
    if LowerCase(language[i].key) = LowerCase(key) then begin
      Result := language[i].value;
      exit;
    end;
  end;

  // maybe try default language?
  for i:=0 to tokenCount - 1 do begin
    if LowerCase(defaultlanguage[i].key) = LowerCase(key) then begin
      Result := defaultlanguage[i].value;
      exit;
    end;
  end;

end;

constructor TParser.Create;
begin
  self.parentTagnames := TStringList.Create;
end;

procedure TParser.ListXmlScannerStartTag(Sender: TObject; TagName: string; Attributes: TAttrList);
begin
  self.parentTagnames.Add(self.currentTagname);
  self.currentTagname := TagName;
end;
procedure TParser.ListXmlScannerEndTag(Sender: TObject; TagName: string);
var lastIndex : integer;
begin
  lastIndex := self.parentTagnames.Count - 1;
  self.currentTagname := self.parentTagnames.Strings[lastIndex];
  self.parentTagnames.Delete(lastIndex);
end;
procedure TParser.ListXmlScannerEmptyTag(Sender: TObject; TagName: string; Attributes: TAttrList);
var lastIndex : integer;
begin
  self.parentTagnames.Add(self.currentTagname);
  self.currentTagname := TagName;
  self.ListXmlScannerContent(Sender, '');
  lastIndex := self.parentTagnames.Count - 1;
  self.currentTagname := self.parentTagnames.Strings[lastIndex];
  self.parentTagnames.Delete(lastIndex);
end;
procedure TParser.ListXmlScannerContent(Sender: TObject; Content: string);
const root: string = 'stringtable';
var key, tn: string; i: integer;
begin
  for i:=0 to self.parentTagnames.Count - 1 do begin
    tn := self.parentTagnames.Strings[i];
    if not (tn = '') and not (tn = root) then key := key + tn + '-';
  end;
  key := key + self.currentTagname;
  Add(key, Content);
end;

end.
