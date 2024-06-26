from setuptools import setup, find_packages

setup(
    name='pycook',
    version='0.12.0',
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
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10'
    ],
    install_requires=[],
    data_files=[('cook', ['README', 'etc/bash-completion.sh', 'cook.el', 'etc/scripts.el', 'etc/elpa.el'])],
    scripts=['etc/cook', 'etc/_cook_complete'],
    entry_points={'console_scripts': [
        'sandbox=pycook.sandbox:main'
    ]}
)
