
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
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.StringTokenizer;


// Class to handle CLASSPATH construction
public class Classpath {
  ArrayList<File> elements = new ArrayList<File>();
  public Classpath() { }
  public Classpath(String initial) { addClasspath(initial); }

  public boolean addComponent(String component) {
    if (component != null && component.length() > 0) {
      try {
        File f = new File(component);
        if (f.exists()) {
          File entry = f.getCanonicalFile();
          if (!elements.contains(entry)) {
            elements.add(entry);
            return true;
          }
        }
      } catch (IOException e) { }
    }
    return false;
  }

  public boolean addComponent(File component) {
    if (component != null) {
      try {
        if (component.exists()) {
          File entry = component.getCanonicalFile();
          if (!elements.contains(entry)) {
            elements.add(entry);
            return true;
          }
        }
      } catch (IOException e) { }
    }
    return false;
  }

  public boolean addClasspath(String s) {
    boolean added = false;
    if (s != null) {
      StringTokenizer t = new StringTokenizer(s, File.pathSeparator);
      while (t.hasMoreTokens())
        added |= addComponent(t.nextToken());
    }
    return added;
  }

  @Override public String toString() {
    StringBuilder cp = new StringBuilder(1024);
    for (File entry : elements)
      cp.append(cp.length() == 0 ? "" : File.pathSeparator).append(entry.getPath());
    return cp.toString();
  }

  public ClassLoader getClassLoader() {
    URL[] urls = new URL[elements.size()];
    int idx = 0;
    for (File entry : elements)
      try { urls[idx++] = entry.toURI().toURL(); } 
      catch (MalformedURLException e) {}
    
    ClassLoader parent = Thread.currentThread().getContextClassLoader();
    if (parent == null) parent = Classpath.class.getClassLoader();
    if (parent == null) parent = ClassLoader.getSystemClassLoader();
    return new URLClassLoader(urls, parent);
  }
}
