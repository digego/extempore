static pointer setDSPClosure(scheme* Scheme, pointer Args)
{
    AudioDevice::I()->setDSPClosure(cptr_value(pair_car(Args)));
    return Scheme->T;
}

static pointer setDSPMTClosure(scheme* Scheme, pointer Args)
{
    AudioDevice::I()->setDSPMTClosure(cptr_value(pair_car(Args)),ivalue(pair_cadr(Args)));
    return Scheme->T;
}

static pointer setDSPWrapper(scheme* Scheme, pointer Args)
{
    AudioDevice::I()->setDSPWrapper(dsp_f_ptr(cptr_value(pair_car(Args))));
    return Scheme->T;
}

static pointer setDSPMTWrapper(scheme* Scheme, pointer Args)
{
    AudioDevice::I()->setDSPMTWrapper(dsp_f_ptr_sum(cptr_value(pair_car(Args))),
            dsp_f_ptr(cptr_value(pair_cadr(Args))));
    return Scheme->T;
}

static pointer initMTAudio(scheme* Scheme, pointer Args)
{
    bool zerolatency = (pair_cadr(Args) == Scheme->T);
    AudioDevice::I()->initMTAudio(ivalue(pair_car(Args)), zerolatency);
    return Scheme->T;
}

static pointer getAudioLoad(scheme* Scheme, pointer Args)
{
    return mk_real(Scheme, double(AudioDevice::getCPULoad()));
}

#define SYS_DSP_DEFS \
        { "sys:set-dsp-closure", &setDSPClosure }, \
        { "sys:set-dspmt-closure", &setDSPMTClosure }, \
        { "sys:set-dsp-wrapper", &setDSPWrapper }, \
        { "sys:set-dspmt-wrapper", &setDSPMTWrapper }, \
        { "sys:init-mt-audio", &initMTAudio }, \
        { "sys:audio-load", &getAudioLoad }
