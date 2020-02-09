# Apophysis 7x DLL renderer
This library exposes the renderer of Apophysis 7x using DLL exports so it can be integrated in other ecosystems. 
This is a rather hacky solution which has been chosen to avoid a tedious and probably pointless translation of Apophysis
into a more integrative (and not completely dead) language.

This source code will not be maintained and using it happens at your own risk. 

Please see https://github.com/xyrus02/apophysis-7x for the full application which probably won't be maintained much either :-)

## Building
To build, I used Embarcardero Delphi XE8. It should be possible with the community edition too but I didn't test it.
If Delphi XE8 is installed on the system and the default installation path has been used, you can simply run `build.cmd` to
create the DLLs.

There is a $!#% ton of warnings which *have always been there(tm)* and I didn't care enough to clean them up yet. Most of them
are "hints", though: unused variables and declarations.

## Exports
The following list contains the DLL exports of this library. All of these are in `StdCall`
convention. Strings should be marshaled as `LPSTR`, pointer sizes are obviously dependent
on the chosen architecture. This project builds Windows x86/x64 architectures, therefore
pointer sizes are 4 bytes / 8 bytes.

### ApophysisInitializeLibrary
This function must be called after linking the library and initializes the globals used
to render the provided flame.

**Parameters**
- none

**Return type**
- void

### ApophysisDestroyLibrary
The counterpart of `ApophysisInitializeLibrary`: releases all allocations made during
initialization and rendering.

**Parameters**
- none

**Return type**
- void

### ApophysisInizializePlugin
Links a DLL plugin so it can be used during rendering.

**Parameters**
- *LPSTR directory*: the path to the plugin directory
- *LPSTR name*: the name (without extension) of the plugin library

**Return type**
- void

### ApophysisSetLogEnabled
Enables or disables log file writing.

**Parameters**
- *DWORD input*: zero to disable logging, any other value to enable it

**Return type**
- void

### ApophysisSetThreadingLevel
Sets the number of threads to be used for the rendering process.

**Parameters**
- *DWORD nthreads*: a value greater than or equal to one determining the number of threads to be used for rendering

**Return type**
- void

### ApophysisGetRegisteredNameCount
Gets the count of known variation names (internal and external/plugin variations)

**Parameters**
- none

**Return type**
- *DWORD*: the amount of known variation names.

### ApophysisGetRegisteredNameAt
Gets the name of a known variation (internal and external/plugin variations) at the given index.

**Parameters**
- *DWORD index*: the index to load the name from
- *LPSTR& buf*: a pointer to an LPSTR buffer to load the name into

**Return type**
- *DWORD*: the amount of characters read into the buffer

### ApophysisGetRegisteredAttribCount
Gets the count of known transform attribute names (like `color`, `color_speed`, ...)

**Parameters**
- none

**Return type**
- *DWORD*: the amount of known transform attribute names.

### ApophysisGetRegisteredAttribAt
Gets the name of a known transform attribute name (like `color`, `color_speed`, ...) at the given index.

**Parameters**
- *DWORD index*: the index to load the name from
- *LPSTR& buf*: a pointer to an LPSTR buffer to load the name into

**Return type**
- *DWORD*: the amount of characters read into the buffer

### ApophysisStartRenderingProcessAndWait
Starts rendering the given flame (see `ParametersSetParameterString`) and copies the resulting image bits onto the given device context. The call will return when the rendering process is done.

**Parameters**
- *DWORD& hDC*: a device context in form of a `uint` pointer

**Return type**
- *DWORD*: the return code of the operation which is zero for success and non-zero for errors.

### ApophysisStartSamplingProcessAndWait
Uses the current point buffer contents to create an image with the data available. This is useful to create an intermediate image or an image after the rendering process has been cancelled. The image bits are copied onto the given device context. 

**Parameters**
- *DWORD& hDC*: a device context in form of a `uint` pointer

**Return type**
- *DWORD*: the return code of the operation which is zero for success and non-zero for errors.

### ApophysisStartSamplingCustomBufferAndWait
Uses the current point buffer contents to create an image from the persited buffer file which has been set before a previous render process using `ParametersSetBufferSavePathString`. The image bits are copied onto the given device context. 

**Parameters**
- *DWORD& hDC*: a device context in form of a `uint` pointer

**Return type**
- *DWORD*: the return code of the operation which is zero for success and non-zero for errors.

### ApophysisStartSlimRenderingProcessAndWait
Starts rendering the given flame (see `ParametersSetParameterString`) using an experimental, memory-preserving ("slim") rendering process and copies the resulting image bits onto the given device context. The call will return when the rendering process is done.

**Parameters**
- *DWORD& hDC*: a device context in form of a `uint` pointer

**Return type**
- *DWORD*: the return code of the operation which is zero for success and non-zero for errors.

### ApophysisCancelRenderingProcess
Cancels the running rendering process.

**Parameters**
- none

**Return type**
- void

### EventsSetOnOperationChangeCallback
Sets the address of a callback function which is invoked whenever a part of the whole process started. 

**Parameters**
- *VOID& cb*: the pointer to the callback function; see list below for function parameters

**Return type**
- void

**Callback function parameters**
- *DWORD op*: the index of the operation which has started at the moment of invocation (`opRendering` = 0, `opHibernating` = 1, `opSampling` = 2)

**Callback function return type**
- void

### EventsSetOnProgressCallback
Sets the address of a callback function which is invoked when the progress of the whole operation changed.

**Parameters**
- *VOID& cb*: the pointer to the callback function; see list below for function parameters

**Return type**
- void

**Callback function parameters**
- *DOUBLE progress*: a value between zero and one determining the progress of the provided slice in the provided batch
- *DWORD slice*: the one-based index of the slice currently rendering in the provided batch
- *DWORD nrslices*: the total count of slices in the provided batch
- *DWORD batch*: the one-based index of the batch currently rendering
- *DWORD nrbatches*: the total count of batches

**Callback function return type**
- void

### EventsSetOnRequestBufferCallback
Sets the address of a callback function which is invoked when the system requires a pointer to a point buffer. This is the case when `ApophysisStartSamplingCustomBufferAndWait` is utilized to render a previously persisted point buffer.

**Parameters**
- *VOID& cb*: the pointer to the callback function; see list below for function parameters

**Return type**
- void

**Callback function parameters**
- *VOID& address*: a pointer to the buffer to be read by Apophysis
- *DWORD x*: the horizontal size of the 2D buffer
- *DWORD y*: the vertical size of the 2D buffer

**Callback function return type**
- void

### ParametersSetParameterString
Sets the parameter string to be used by the renderer.

**Parameters**
- *LPSTR input*: the XML string to be read by Apophysis

**Return type**
- void

### ParametersSetLogSavePathString
Sets the path to the folder where Apophysis saves its log files.

**Parameters**
- *LPSTR input*: the directory path to be used

**Return type**
- void

### ParametersSetBufferSavePathString
Sets the path to the file where Apophysis saves the point buffer.

**Parameters**
- *LPSTR input*: the file path to be used

**Return type**
- void

### ParametersSetImagePaths
Sets the path to the file where Apophysis saves the target image and the alpha mask.

**Parameters**
- *LPSTR image*: the file path to be used for the target image
- *LPSTR alpha*: the file path to be used for the alpha mask

**Return type**
- void

### ParametersSetOutputDimensions
Sets the horizontal and vertical image dimensions for the target image.

**Parameters**
- *DWORD x*: the horizontal dimension
- *DWORD y*: the vertical dimension

**Return type**
- void

### ParametersSetSamplingParameters
Sets the OSAA (oversampling anti-aliasing) parameters.

**Parameters**
- *DWORD os*: the degree of oversampling (x times the image size); must be larger than zero
- *DOUBLE fr*: the radius of the gaussian blur filter which is applied to the oversampled image before the size is reduced to the target image size

**Return type**
- void

### ParametersSetSamplesPerPixel
Sets the amount of iterations per pixel (also called "density" or "quality")

**Parameters**
- *DOUBLE input*: the desired amount of iterations per pixel

**Return type**
- void

### ParametersSetVibrancy
Sets the color vibrancy of the target image

**Parameters**
- *DOUBLE input*: the desired vibrancy
**Return type**
- void

### ParametersUpdateDependencies
Recalculate all values which are dependent on other, possibly changed values. This should be called directly before rendering if the parameters were changed in memory.

**Parameters**
- none
**Return type**
- void


## Editing parameters
These exports are used to manipulate the flame loaded into memory by `ParametersSetParameterString`. The signatures of these functions are pretty much all the same so they are not explained in detail like the exports above. Instead, pseudocode is used to illustrate the signatures.

    // flame operations
    void FLReset();                // clears all parameters
    void FLApplyFinal();           // turns the last transform into a final one

    // export
    int FLToString(LPSTR& buffer); // saves the parameters to XML, returns the char count
    int FLRandomToFile(            // generates a random batch and saves it to file
        LPSTR fileName,               // target filename
        int batchSize,                // batch size
        int width,                    // batch item image width
        int height                    // batch item image width
    );

    // flame parameters
    void FLName(LPSTR v);                   // display name (used for image metadata)
    void FLBrightness(double v);            // brightness
    void FLVibrancy(double v);              // vibrancy
    void FLGamma(double v);                 // gamma
    void FLGammaThreshold(double v);        // gamma threshold
    void FLOversample(int v);               // OSAA degree
    void FLFilter(double v);                // OSAA filter radius
    void FLZoom(double v);                  // logarithmic zoom ratio (ratio = 2^zoom)
    void FLScale(double v);                 // units per pixel ratio ("fast zoom")
    void FLQuality(double v);               // iterations per pixel ("quality", "density")
    void FLSize(double x, double y);        // image size (used to calculate aspect ratio)
    void FLBackground(int r, int g, int b); // image background color

    // palette
    void FLCMap(int i, int r, int g, int b); // palette color (i = index)
    
    // camera
    void FLCenter(double x, double y);      // 2D camera X/Y-position
    void FLAngle(double v);                 // 2D camera rotation angle in degrees (legacy)
    void FLRotate(double v);                // 2D camera rotation angle in radians 
    void FLCamZPos(double v);               // 3D camera Z-position
    void FLCamPitch(double v);              // 3D camera pitch in radians
    void FLCamYaw(double v);                // 3D camera yaw in radians
    void FLCamPerspective(double v);        // 3D camera perspective distortion ratio
    void FLCamDist(double v);               // 3D camera distance (= 1 / perspective)
    void FLCamDof(double v);                // 3D camera depth of field ratio

    // density estimation (experimental)
    void FLEnableDE(int v);                 // enable/disable (1/0) density estimation
    void FLEstimatorRadius(double v);       // DE blur radius
    void FLEstimatorMinimum(double v);      // DE minimum blur radius
    void FLEstimatorCurve(double v);        // DE blur radius curve coefficient

    // transform settings (first parameter is always the zero-based transform index)
    void FLSoloXForm(int index);                  // disable all but this transform
    void FLXName(int index, LPSTR v);             // name (only descriptive)
    void FLXWeight(int index, double v);          // global probability of choice
    void FLXChaos(int iFrom, int iTo, double v);  // relative probability of choice
    void FLXCoefs(int index, int ic, double v);   // affine coefficient (ic = matrix index)
    void FLXPost(int index, int ic, double v);    // post transform coefficient
    void FLXColor(int index, double v);           // color coefficient
    void FLXColorSpeed(int index, double v);      // color spread coefficient
    void FLXVarColor(int index, double v);        // variation color weight
    void FLXVar(int index, LPSTR name double v);  // variation weight or parameter value

