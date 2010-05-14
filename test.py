import glob
import os.path
import sys
import unittest

TESTS_FOLDER = 'tests'
sys.path.insert(0, TESTS_FOLDER)

if len(sys.argv) > 1:
    test_modules = ['test-' + name for name in sys.argv[1:]]
    # unbelievable hack to avoid a totally obscure error from unittest.main
    sys.argv = sys.argv[:1]
else:
    test_modules = []
    for path in glob.glob(os.path.join(TESTS_FOLDER, 'test-*.py')):
        filename = os.path.split(path)[1]
        test_modules.append(os.path.splitext(filename)[0])

for module in test_modules:
    unittest.main(module=module, exit=False)
