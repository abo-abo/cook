#!/usr/bin/env python

from setuptools import setup, find_packages

setup(
    name='pycook',
    version='0.5.14',
    description='Makefile -> Cookbook.py.',
    long_description='https://raw.githubusercontent.com/abo-abo/cook/master/README.org',
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
    data_files=[('cook', ['README', 'etc/bash-completion.sh', 'etc/scripts.el'])],
    entry_points={'console_scripts': ['cook=pycook.cook:main', '_cook_complete=pycook.cook:complete']}
)
