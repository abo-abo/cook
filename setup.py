#!/usr/bin/env python

from setuptools import setup, find_packages
from codecs import open
import os

here = os.path.abspath(os.path.dirname(__file__))

try:
    with open(os.path.join(here, 'README.org'), encoding='utf-8') as f:
        long_description = f.read()
except Exception:
    long_description("Couldn't find README.org")

setup(
    name='pycook',
    version='0.1.2',
    description='Makefile -> Cookbook.py.',
    long_description=long_description,
    packages=find_packages(exclude=['etc', 'examples']),
    url='https://github.com/abo-abo/cook',
    author='Oleh Krehel',
    author_email='ohwoeowho@gmail.com',
    license='GPLv3+',
    keywords='makefile automation',
    # See https://pypi.python.org/pypi?%3Aaction=list_classifiers
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'Topic :: Software Development :: Build Tools',
        'License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5'],
    install_requires=[],
    data_files=[('cook', ['README.org', 'etc/bash-completion.sh'])],
    entry_points={'console_scripts': ['cook=pycook.__main__:main']}
)
