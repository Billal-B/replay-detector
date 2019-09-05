from setuptools import find_packages
from setuptools import setup

REQUIRED_PACKAGES = ['tensorflow', 'keras', 'google-cloud-storage', 'Pillow']

setup(
    name='replay-trainer',
    version='0.1',
    install_requires=REQUIRED_PACKAGES,
    packages=find_packages(),
    include_package_data=True,
    description='Learn to detect replays on videos'
)
