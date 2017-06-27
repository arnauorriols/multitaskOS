document.addEventListener('DOMContentLoaded', function() {
    function compat(model) {
        for (props of [['thread', 'job'], ['threadQueue', 'jobQueue'], ['newThread', 'newJob']]) {
            var oldProp = props[0];
            var newProp = props[1];
            if (model.hasOwnProperty(oldProp)) {
                model[newProp] = model[oldProp];
                delete model[oldProp];
            }
        }

				// v.1.2.0
			  let migrateJob = (job) => {
						job.title = job.threadName;
						delete job.threadName;
						if (job.worklog) {
								job.journal.unshift(job.worklog);
						}
						job.worklog = job.journal;
						delete job.journal;
						return job;
				}

        for (props of [['newJob', 'unsavedJob']]) {
            var oldProp = props[0];
            var newProp = props[1];
            if (model.hasOwnProperty(oldProp)) {
                model[newProp] = model[oldProp];
                delete model[oldProp];
            }
        }

			  if (model.unsavedJob.hasOwnProperty('threadName')) {
					  model.usavedJob = migrateJob(model.unsavedJob);
				}

				if (model.hasOwnProperty('job')) {
						if (model.job) {
							  model.jobQueue.unshift(model.job);
						}
					  delete model.job;
				}

				if (model.jobQueue.length && model.jobQueue[0].hasOwnProperty('threadName')) {
					  model.jobQueue = model.jobQueue.map(migrateJob);
				}

				if (model.jobQueue.length && !(model.jobQueue[0] instanceof Array)) {
					  model.jobQueue = model.jobQueue.map((job) => ["Queued", job]);
				}
				console.log(model.jobQueue);
				console.log(model.jobQueue[0]);
				console.log(model.jobQueue[0] instanceof Array);

			  if (model.hasOwnProperty('hotkeysPressed')) {
						delete model.hotkeysPressed;
				}
        return model;
    }
    var STORAGE_KEY = 'MultitaskOS-Model';
    var storageContent = localStorage.getItem(STORAGE_KEY);
    var savedModel =
        storageContent ?
          compat(JSON.parse(storageContent)):
          null;
    var multitaskos = Elm.Main.embed(document.getElementById('elm-app'), savedModel);
    multitaskos.ports.persistModel.subscribe(function (model) {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(model));
    });
});
