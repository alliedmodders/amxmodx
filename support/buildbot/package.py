#!/usr/bin/env python3
# vim: set ts=4 sw=4 tw=99 et:
import argparse
import datetime
import ftplib
import os
import platform
import re
import shutil
import subprocess
import time

kPackages = ['base', 'cstrike', 'dod', 'esf', 'ns', 'tfc', 'ts']
kMacExclude = set(['esf', 'ns', 'ts'])

class PackageBuilder(object):
    def __init__(self, args):
        self.args_ = args

        if platform.system() == 'Linux':
            self.os_ = 'linux'
        elif platform.system() == 'Windows':
            self.os_ = 'windows'
        elif platform.system() == 'Darwin':
            self.os_ = 'mac'
        else:
            raise Exception('Unknown platform: {}'.format(platform.system()))

        if self.os_ == 'linux':
            self.archive_type_ = 'tar.gz'
        else:
            self.archive_type_ = 'zip'
        self.packages_ = []

    def run(self):
        self.read_ftp_info()
        self.read_product_version()

        # Switch to the output folder.
        with Chdir(os.path.join('..', 'OUTPUT')):
            self.build()
            self.upload()

    def build(self):
        print("Creating package for {}".format(self.version_))

        for package in kPackages:
            if self.os_ == 'mac' and package in kMacExclude:
                continue

            package_dir = os.path.join('packages', package)
            with Chdir(package_dir):
                package_name, alt_name = self.build_package(package)

            if os.path.exists(package_name):
                os.unlink(package_name)

            src_path = os.path.join(package_dir, package_name)
            shutil.move(src_path, package_name)

            self.packages_.append((package_name, alt_name))

    def upload(self):

        m = re.search("^(\d+)\.(\d+)", self.version_)
        ftp_path = '{}/{}.{}'.format(self.ftp_path_, m.group(1), m.group(2))

        print("Connecting to drop site: {}".format(ftp_path))
        ftp = ftplib.FTP(self.ftp_host_)
        ftp.login(self.ftp_user_, self.ftp_pass_)
        ftp.set_pasv(True)
        ftp.cwd(ftp_path)

        for package_file, alt_file in self.packages_:
            self.upload_package(ftp, package_file, package_file)
            self.upload_package(ftp, package_file, alt_file)

        print("Files sent to drop site -- build succeeded.")

    def upload_package(self, ftp, package_file, dest_name):
        with open(package_file, 'rb') as fp:
            print("Sending {} as {}".format(package_file, dest_name))
            ftp.storbinary('STOR {}'.format(dest_name), fp)

    def build_package(self, package):
        package_file = 'amxmodx-{}-{}-{}.{}'.format(self.version_, package, self.os_,
                                                    self.archive_type_)
        if os.path.exists(package_file):
            os.unlink(package_file)

        if self.archive_type_ == 'tar.gz':
            Run(['tar', 'zcvf', package_file, 'addons'])
        else:
            Run(['zip', '-r', package_file, 'addons'])

        alt_name = 'amxmodx-latest-{}-{}.{}'.format(package, self.os_, self.archive_type_)
        return package_file, alt_name

    def read_ftp_info(self):
        with open(self.args_.ftp_file, 'rt') as fp:
            lines = [line.strip() for line in fp.read().splitlines()]
            self.ftp_host_ = lines[0]
            self.ftp_user_ = lines[1]
            self.ftp_pass_ = lines[2]
            self.ftp_path_ = lines[3]

    def read_product_version(self):
        with open('product.version', 'rt') as fp:
            self.version_ = fp.read().strip()

        output = subprocess.check_output(['git', 'rev-list', '--count', 'HEAD'],
                                         universal_newlines = True,
                                         stderr = subprocess.STDOUT)
        output = output.strip()
        if output:
            self.version_ += '-git' + output

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('ftp_file', type = str, help = 'FTP config file')

    args = parser.parse_args()

    builder = PackageBuilder(args)
    builder.run()

class Chdir(object):
    def __init__(self, target):
        self.prevdir_ = None
        self.target_ = target

    def __enter__(self):
        prevdir = os.getcwd()
        os.chdir(self.target_)
        self.prevdir_ = prevdir
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        if self.prevdir_:
            os.chdir(self.prevdir_)
        return False

def Run(argv):
    try:
        output = subprocess.check_output(argv, stderr = subprocess.STDOUT,
                                         universal_newlines = True)
        print(output)
    except subprocess.CalledProcessError as e:
        print(e.output.decode('utf-8'))
        raise

if __name__ == '__main__':
    main()
