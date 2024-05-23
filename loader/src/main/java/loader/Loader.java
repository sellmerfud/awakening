
// Copyright (c) 2021 Curt Sellmer
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package loader;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.lang.reflect.Method;
import java.security.Policy;

// This is a boostrap loader for loading the main application.
// To simplify loading the server this lightweight class
// creates a classpath with all necessary jar files then instantiates a
// new class loader loads the main program.

public class Loader {

  public static void main(String[] args) {
    Loader instance = new Loader();
    System.exit(instance.run(args));
  }
  
  private int run(String[] args) {
    try {
      return startApplication(args);
    }
    catch (Error e) {
      String msg = e.getMessage();
      if (msg != null && msg.length() > 0)
        System.out.println(msg);
      return e.returnCode;
    }
    catch (Throwable e) {
      System.out.println(rootCause(e).getMessage());
      return 1;
    }
  }
  
  private void error(String msg) {
    throw new Error(msg);
  }
  
  private void error(String msg, int rc) {
    throw new Error(msg, rc);
  }
  
  private Throwable rootCause(Throwable e) {
    if (e.getCause() == null || e.getCause() == e) return e; else return rootCause(e.getCause());
  }
  
  class Error extends RuntimeException {
    public final int returnCode;
    public Error(String s, int rc) {
      super(s);
      returnCode = rc;
    }
    public Error(String s) {
      this(s, 1);
    }
  }


  //  This code assumes that the current working directory
  //  is the install directory which should contain a subdirctory
  //  call 'lib' which contains the jar files needed to run the application.
  //  The script that invokes this Loader is responsible for doing that.
  private int startApplication(String[] args) {
    String app_class = System.getProperty("loader.targetClass");
    if (app_class == null) 
      error("loader.targetClass property is not defined.");
      
    File libDir = new File("./lib");
    if (!libDir.isDirectory())
      error("Cannot file ./lib directory");
    
    //  Resolve the path to an absolute path and remove and /../ type  parts.
    try { libDir = libDir.getCanonicalFile(); }
    catch (IOException e2) {
      error("Error getting canonical lib path");
    }
    
    //  First we must build up the class path with all files in the lib directory.
    Classpath cp = new Classpath();
    addJarFilesToClasspath(cp, libDir);

    // Get a new classloader using our newly created classpath and set it as the
    // context loader for the current thread.
    ClassLoader cl = cp.getClassLoader();
    Thread.currentThread().setContextClassLoader(cl);
    // re-eval the policy now that the environment is set
    try {
       Policy policy = Policy.getPolicy();
       if (policy != null)
       policy.refresh();
    }
    catch (Exception e) {
      error("Unable refresh security policy: " + e.getMessage());
    }

    Class<?> applicationClass = null;
    try { applicationClass = cl.loadClass(app_class); }
    catch (ClassNotFoundException e) {
      error("Unable to load " + app_class + ": " + e.getMessage());
   }
        
    // We have successfully loaded the class, now use Reflection
    // get refrences to the main() method and call it with the command line
    // arguments.
    try {
      Class<?>[] appMainTypes = new Class[]  { args.getClass() };
      Object[] params         = new Object[] { args };
      Method appMain          = applicationClass.getDeclaredMethod("main", appMainTypes);
      appMain.invoke(null, params);
      return 0;
    }
    catch (Exception e) {
      e.printStackTrace();
      error("Unable to invoke " + app_class + ".main(): " + e.getMessage());
      return 1;
    }
  }
  
  
  // Add to the given classpath all of the jar files found
  // in the given libDir including all subdirectories.
  private void addJarFilesToClasspath(Classpath cp, File libDir) {
    FileFilter filter = new FileFilter() {
      public boolean accept(File candidate) {
        return candidate.isDirectory() || candidate.getName().endsWith(".jar");
      }
    };
    
    File[] fileList = libDir.listFiles(filter);
    for (File file : fileList)
      if (file.isDirectory())
        addJarFilesToClasspath(cp, file);
      else
        cp.addComponent(file);
  }  
}
