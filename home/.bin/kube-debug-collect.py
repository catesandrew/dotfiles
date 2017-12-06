#!/usr/bin/env python3

import json
import os
import tempfile
import time

from datetime import datetime
from subprocess import check_output, check_call, CalledProcessError, Popen


def debug_action(temppath, status, application):
    # FIXME: doesn't handle subordinate apps properly yet
    # FIXME: these could be done in parallel
    apps = status.get('applications', {})
    app = apps.get(application, {})
    units = app.get('units', {})
    for unit in list(units.keys()):
        print('Executing debug action on %s...' % unit)
        cmd = 'juju run-action %s debug --format json' % unit
        try:
            raw_action = check_output(cmd.split())
            action = json.loads(raw_action.decode())
        except:
            print('Error running the debug action. Skipping.')
            continue
        action_id = action['Action queued with id']
        while True:
            # FIXME: blocks forever in a couple cases
            cmd = 'juju show-action-output %s --format json' % action_id
            try:
                raw_action_output = check_output(cmd.split())
                action_output = json.loads(raw_action_output.decode())
            except:
                print('Error checking action output. Ignoring.')
                continue
            if action_output['status'] in ['running', 'pending']:
                time.sleep(1)
                continue
            if action_output['status'] == 'completed':
                unit_name = unit.split('/')[0]
                unit_id = unit.split('/')[1]
                outpath = os.path.join(temppath, 'debug', unit_name, unit_id)
                os.makedirs(outpath)
                cmd = 'juju scp %s:%s %s' % (unit, action_output['results']['path'], outpath)
                try:
                    check_call(cmd.split())
                except:
                    print('Error copying debug action output. Skipping.')
                break
            print('Failed debug action on unit %s, status %s' % (unit, action_output['status']))
            break


def command(temppath, filename, cmd):
    print('Running %s...' % cmd)
    path = os.path.join(temppath, filename)
    stdout = open(path+'.out', 'w')
    stderr = open(path+'.err', 'w')
    proc = Popen(cmd.split(), stdout=stdout, stderr=stderr)
    proc.wait()
    stdout.close()
    stderr.close()


def store_results(temppath):
    ts = datetime.now().strftime("%Y-%m-%d-%H-%M-%S")
    fname = 'results-%s.tar.gz' % ts
    cmd = 'tar -C %s -czf %s results' % (os.path.dirname(temppath), fname)
    check_call(cmd.split())
    print('Results stored in %s.' % fname)


def main():
    tempdir = tempfile.TemporaryDirectory()
    temppath = os.path.join(tempdir.name, 'results')
    os.makedirs(temppath)

    print('Getting juju status...')
    try:
        raw_status = check_output('juju status --format json'.split())
        status = json.loads(raw_status.decode())
    except:
        print('Error getting juju status. Aborting.')
        return

    debug_action(temppath, status, 'kubernetes-master')
    debug_action(temppath, status, 'kubernetes-worker')
    debug_action(temppath, status, 'etcd')
    # FIXME: no debug action on kubeapi-load-balancer, easyrsa, flannel

    command(temppath, 'status', 'juju status --format yaml')
    command(temppath, 'debug-log', 'juju debug-log --replay')
    command(temppath, 'storage', 'juju storage --format yaml')
    command(temppath, 'storage-pools', 'juju storage-pools --format yaml')
    command(temppath, 'kubernetes-master-config', 'juju config kubernetes-master --format yaml')
    command(temppath, 'kubernetes-worker-config', 'juju config kubernetes-worker --format yaml')
    command(temppath, 'kubeapi-load-balancer-config', 'juju config kubeapi-load-balancer --format yaml')
    command(temppath, 'etcd-config', 'juju config etcd --format yaml')
    command(temppath, 'easyrsa-config', 'juju config easyrsa --format yaml')
    command(temppath, 'flannel-config', 'juju config flannel --format yaml')

    store_results(temppath)


if __name__ == '__main__':
    main()
