function result=h5load(varargin)
% result = H5LOAD(filename[, path])
%
% Load data from a HDF5 file to a (possibly nested) struct array.
%

%
% This is an example Matlab script that uses Pythoncall.
%

x = {
'import numpy as N';
'import tables, gc';
'def walk_hdf5_tree(n):';
'    if isinstance(n, tables.Group):';
'        o = {}';
'        for name, item in n._v_children.iteritems():';
'            if name.startswith("_"): continue';
'            o[name] = walk_hdf5_tree(item)';
'        return o';
'    elif isinstance(n, tables.Array):';
'        return N.asarray(n.read())';
'    elif isinstance(n, tables.Table):';
'        o = {}';
'        for colname in n.colnames:';
'            o[colname] = N.asarray(n.col(colname))';
'        return o';
'    else:';
'        return None';
'';
'f = tables.openFile(filename, "r")';
'try: result = walk_hdf5_tree(f.getNode(path))';
'finally: f.close()';
};

s = '';
for k=1:length(x),
  s = sprintf('%s\n%s', s, x{k});
end

if nargin == 0
  error('Too few arguments');
end
if nargin >= 1
  filename = varargin{1};
end
if nargin >= 2
  path = varargin{2};
else
  path = '/';
end
if nargin > 2
  error('Too many arguments');
end

pythoncall('set', 'path', path);
pythoncall('set', 'filename', filename);
pythoncall('eval', s);
result = pythoncall('get', 'result');
pythoncall('eval', 'del result; gc.collect()');
